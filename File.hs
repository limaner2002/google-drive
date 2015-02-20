{-# LANGUAGE OverloadedStrings #-}
module File
    where

import Data.Aeson
import Control.Applicative ((<$>),(<*>))
import Data.Maybe
import Control.Monad (mzero, void)
import qualified Data.ByteString.Lazy as BL

data File = File
          { name :: String
          } deriving (Show)

instance FromJSON File
    where
      parseJSON (Object o) = File <$> o .: "title"
      parseJSON _ = mzero
