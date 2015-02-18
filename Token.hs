{-# LANGUAGE OverloadedStrings #-}
module Token
    where

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

import Data.Aeson
import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero, void)
import Data.Maybe

data Token = Token
    { accessToken :: !String,
      expiration :: !Int,
      tokenType :: !String,
      refreshToken :: !String
    } deriving Show

decode :: BL.ByteString -> Token
decode j = fromJust (Data.Aeson.decode j :: Maybe Token)

instance FromJSON Token where
    parseJSON (Object v) = Token <$>
                           v .: "access_token" <*>
                           v .: "expires_in" <*>
                           v .: "token_type" <*>
                           v .: "refresh_token"
    parseJSON _ = mzero
