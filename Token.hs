{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Token
    where

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

import Data.Aeson
import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero, void)
import Data.Maybe
import GHC.Generics

data Token = Token
    { accessToken :: !String,
      expiration :: !Int,
      tokenType :: !String,
      refreshToken :: Maybe String
    } deriving (Show, Generic)

decode :: BL.ByteString -> Token
decode j = fromJust (Data.Aeson.decode j :: Maybe Token)

save :: FilePath -> Token -> IO ()
save fileName tok = BL.writeFile fileName encoded
    where
      encoded = Data.Aeson.encode tok

load :: FilePath -> IO (Maybe Token)
load fName = do
  json <- BS.readFile fName
  return (Data.Aeson.decodeStrict json :: Maybe Token)

instance FromJSON Token where
    parseJSON (Object v) = Token <$>
                           v .: "access_token" <*>
                           v .: "expires_in" <*>
                           v .: "token_type" <*>
                           v .:? "refresh_token"
    parseJSON _ = mzero

instance ToJSON Token where
    toJSON (Token accessToken expiration tokenType refreshToken) =
        object [ "access_token" .= accessToken,
                 "expires_in" .= expiration,
                 "token_type" .= tokenType,
                 "refresh_token" .= refreshToken
               ]
