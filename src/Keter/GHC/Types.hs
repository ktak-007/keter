{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Keter.GHC.Types
  ( SOHandles(..)
  ) where

import Control.DeepSeq
import Data.Text (Text)
import GHC.Generics

-- | The set of functions that you want to expose from your shared object
data SOHandles = SOHandles
    { soInfo :: Maybe Text
    , soInitialize :: Int -> IO ()
    , soMiddleware :: Int -> IO ()
    } deriving (Generic, NFData)

instance Show SOHandles where
    show (SOHandles {..}) = "SOHandles{soInfo=" <> show soInfo <> ",..}"
