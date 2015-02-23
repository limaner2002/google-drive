{-# LANGUAGE OverloadedStrings #-}
module Token
    where

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

import Data.Aeson
import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero, void)
import Data.Maybe

import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.Ptr (Ptr,nullPtr)

foreign import ccall "StorePasswordKeychain" c_StorePasswordKeychain :: Ptr CChar -> Int -> Ptr CChar ->
                                                                        Int-> Ptr CChar -> Int -> Int

data Token = Token
    { accessToken :: !String,
      expiration :: !Int,
      tokenType :: !String,
      refreshToken :: Maybe String
    } deriving (Show)

save :: FilePath -> Maybe Token -> IO ()
save _ Nothing = putStrLn "Cannot save Nothing"
save fileName (Just tok) = BL.writeFile fileName encoded
    where
      encoded = Data.Aeson.encode tok

save_ :: String -> String -> String -> IO ()
save_ service account pass = do
  cService <- newCStringLen service
  cAccount <- newCStringLen account
  cPass <- newCStringLen pass
  let result = c_StorePasswordKeychain svc svclen acct acctlen passwd passlen
          where
            (passwd, passlen) = cPass
            (svc, svclen) = cService
            (acct, acctlen) = cAccount
  putStrLn $ show result


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
