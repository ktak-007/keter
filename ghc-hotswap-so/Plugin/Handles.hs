{-# LANGUAGE OverloadedStrings #-}

module Plugin.Handles
  ( hsNewSOHandle
  ) where

import           Foreign
import           Keter.GHC.Types
import qualified Plugin.Body

foreign export ccall "hs_soHandles"
  hsNewSOHandle :: IO (StablePtr SOHandles)

hsNewSOHandle :: IO (StablePtr SOHandles)
hsNewSOHandle = newStablePtr SOHandles
  { soInfo = Just "I live in a shared object"
  , soInitialize = Plugin.Body.initialize
  , soMiddleware = Plugin.Body.middleware
  }
