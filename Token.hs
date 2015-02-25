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

import System.IO

foreign import ccall "StorePasswordKeychain" c_StorePasswordKeychain :: Ptr CChar -> Int -> Ptr CChar ->
                                                                        Int-> Ptr CChar -> Int -> Int
foreign import ccall "GetPasswordKeychain" c_GetPasswordKeychain :: Ptr a -> CUInt -> Ptr a ->
                                                                    CUInt -> CString

data Token = Token
    { accessToken :: !String,
      expiration :: !Int,
      tokenType :: !String,
      refreshToken :: Maybe String
    } deriving (Show)

type PageToken = String

save :: Maybe Token -> IO ()
save Nothing = putStrLn "Cannot save Nothing"
save (Just tok) = do
  saveRefreshToken "My Google Drive" "MyDrive" (refreshToken tok)
  BL.writeFile "token" encoded
    where
      encoded = Data.Aeson.encode tok

saveRefreshToken :: String -> String -> Maybe String -> IO ()
saveRefreshToken _ _ Nothing = hPutStrLn stderr "No refresh token to save!"
saveRefreshToken service account (Just pass) = do
  cService <- newCStringLen service
  cAccount <- newCStringLen account
  cPass <- newCStringLen pass
  let result = c_StorePasswordKeychain svc svclen acct acctlen passwd passlen
          where
            (passwd, passlen) = cPass
            (svc, svclen) = cService
            (acct, acctlen) = cAccount
  putStrLn $ show result

fromKeychain :: String -> String -> IO (Maybe String)
fromKeychain service account = do
  cService <- newCStringLen service
  cAccount <- newCStringLen account
  let result = c_GetPasswordKeychain svc (fromIntegral svclen) acct (fromIntegral acctlen)
          where
            (svc, svclen) = cService
            (acct, acctlen) = cAccount
  -- Convert from c-space to haskell space
  checkResult result

checkResult :: CString -> IO (Maybe String)
checkResult c_str
    | c_str == nullPtr = return Nothing
    | otherwise = do
                  result <- peekCString c_str
                  return $ Just result

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
                 "token_type" .= tokenType
               ]
