{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Keter.Config.Middleware where

import Data.Aeson
import GHC.Generics
import Network.Wai
import Prelude

import Control.Arrow ((***))
import Control.Exception (throwIO)
import Control.Monad

-- various Middlewares
import Network.Wai.Middleware.AcceptOverride (acceptOverride)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.HttpAuth (basicAuth)
import Network.Wai.Middleware.Jsonp (jsonp)
import Network.Wai.Middleware.Local (local)
import Network.Wai.Middleware.MethodOverride (methodOverride)
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (fromString)
import Data.Text (unpack)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Keter.Aeson.KeyHelper as AK (empty, toKey, toList, toText)
import Keter.Common (KeterException(ExecutableNotFound))
import Keter.GHC.Types (SOHandles(..))

-- Rate limiter (updated API: buildEnvFromConfig + buildRateLimiterWithEnv)
import Keter.RateLimiter.WAI
  ( RateLimiterConfig(..)
  , buildEnvFromConfig
  , buildRateLimiterWithEnv
  )

data MiddlewareConfig = AcceptOverride
                      | Autohead
                      | Jsonp
                      | MethodOverride
                      | MethodOverridePost
                      | AddHeaders ![(S.ByteString, S.ByteString)]
                      | BasicAuth !String ![(S.ByteString, S.ByteString)] 
                         -- ^ Realm [(username,password)]
                      | Local !Int !L.ByteString
                         -- ^ Status Message
                      | RateLimiter !RateLimiterConfig
                         -- ^ Rate Limiter
                      | Custom ![S.ByteString]
          deriving (Show, Generic)

instance FromJSON MiddlewareConfig where
  parseJSON (String "accept-override"     ) = pure AcceptOverride
  parseJSON (String "autohead"            ) = pure Autohead
  parseJSON (String "jsonp"               ) = pure Jsonp
  parseJSON (String "method-override"     ) = pure MethodOverride
  parseJSON (String "method-override-post") = pure MethodOverridePost
  parseJSON (Object o) =
    case AK.toList o of
      [("basic-auth", Object o')] ->
        BasicAuth  <$> o' .:? "realm" .!= "keter"
                   <*> (map ((T.encodeUtf8 . AK.toText) *** T.encodeUtf8)
                       . AK.toList <$> o' .:? "creds" .!= AK.empty)

      [("headers", Object _ )] ->
        AddHeaders . map ((T.encodeUtf8 . AK.toText) *** T.encodeUtf8)
          . AK.toList <$> o .:? "headers" .!= AK.empty

      [("local", Object o')] ->
        Local <$> o' .:? "status" .!= 401
              <*> (TL.encodeUtf8 <$> o' .:? "message"
                   .!= "Unauthorized Accessing from Localhost ONLY")

      [("rate-limiter", v)] -> RateLimiter <$> parseJSON v

      [("custom", v)] -> Custom . map T.encodeUtf8 <$> parseJSON v

      _ -> mzero
  parseJSON _ = mzero

instance ToJSON MiddlewareConfig where
  toJSON AcceptOverride     = "accept-override"
  toJSON Autohead           = "autohead"
  toJSON Jsonp              = "jsonp"
  toJSON MethodOverride     = "method-override"
  toJSON MethodOverridePost = "method-override-post"
  toJSON (BasicAuth realm cred) =
    object [ "basic-auth" .= object
              [ "realm" .= realm
              , "creds" .= object
                  (map ((AK.toKey . T.decodeUtf8) *** (String . T.decodeUtf8)) cred)
              ] ]
  toJSON (AddHeaders headers) =
    object [ "headers" .= object
              (map ((AK.toKey . T.decodeUtf8) *** String . T.decodeUtf8) headers) ]
  toJSON (Local sc msg) =
    object [ "local" .= object [ "status" .= sc
                               , "message" .= TL.decodeUtf8 msg ] ]
  toJSON (RateLimiter rl) = object [ "rate-limiter" .= toJSON rl ]

  toJSON (Custom custom) = object ["custom" .= map T.decodeUtf8 custom]

{-- Still missing
-- Clean path
-- Gzip
-- RequestLogger
-- Rewrite
-- Vhost
--}

-- Build middlewares in IO, because the RateLimiter needs mutable state.
processMiddlewareIO :: Map S.ByteString SOHandles -> [MiddlewareConfig] -> IO Middleware
processMiddlewareIO runningSoLibs cfgs = do
  mws <- mapM (toMiddlewareIO runningSoLibs) cfgs
  pure (composeMiddleware mws)

processMiddleware :: [MiddlewareConfig] -> Middleware
processMiddleware = composeMiddleware . map toMiddlewarePure
  where
    toMiddlewarePure (RateLimiter _) = id
    toMiddlewarePure x               = unsafeToMiddleware x

composeMiddleware :: [Middleware] -> Middleware
composeMiddleware = foldl (flip (.)) id

unsafeToMiddleware :: MiddlewareConfig -> Middleware
unsafeToMiddleware AcceptOverride     = acceptOverride
unsafeToMiddleware Autohead           = autohead
unsafeToMiddleware Jsonp              = jsonp
unsafeToMiddleware MethodOverride     = methodOverride
unsafeToMiddleware MethodOverridePost = methodOverridePost
unsafeToMiddleware (Local s c)        = local (responseLBS (toEnum s) [] c)
unsafeToMiddleware (BasicAuth realm cred) =
  basicAuth (\u p -> pure $ (Just p ==) $ lookup u cred) (fromString realm)
unsafeToMiddleware (AddHeaders headers) = addHeaders headers
unsafeToMiddleware (RateLimiter _)      = id

toMiddlewareIO :: Map S.ByteString SOHandles -> MiddlewareConfig -> IO Middleware
toMiddlewareIO _ AcceptOverride     = pure acceptOverride
toMiddlewareIO _ Autohead           = pure autohead
toMiddlewareIO _ Jsonp              = pure jsonp
toMiddlewareIO _ MethodOverride     = pure methodOverride
toMiddlewareIO _ MethodOverridePost = pure  methodOverridePost
toMiddlewareIO _ (Local s c)        = pure $ local (responseLBS (toEnum s) [] c)
toMiddlewareIO _ (BasicAuth realm cred) =
  pure $ basicAuth (\u p -> pure $ (Just p ==) $ lookup u cred) (fromString realm)
toMiddlewareIO _ (AddHeaders headers) = pure $ addHeaders headers
toMiddlewareIO _ (RateLimiter rl) = do
  env <- buildEnvFromConfig rl
  pure (buildRateLimiterWithEnv env)
toMiddlewareIO runningSoLibs (Custom c) = composeMiddleware <$> mapM (customMiddleware runningSoLibs) c

customMiddleware :: Map S.ByteString SOHandles -> S.ByteString -> IO Middleware
customMiddleware runningSoLibs name = do
  SOHandles {..} <- maybe
    (throwIO $ ExecutableNotFound $ "Shared library " <> unpack (T.decodeUtf8 name) <>  " not found")
    pure
    (Map.lookup name runningSoLibs)
  return $ \app req respond ->
    middleware 1 >> app req respond
