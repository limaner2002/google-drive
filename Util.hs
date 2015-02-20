{-# LANGUAGE OverloadedStrings #-}
module Util
    where

import Data.Aeson
import Data.String
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import System.IO
import Control.Exception
import Control.Arrow (second)
import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as C8

getL :: (FromJSON a) => BL.ByteString -> IO (Maybe a)
getL "" = return Nothing
getL string = do
    let result = (Data.Aeson.decode string :: (FromJSON a) => Maybe a)
    case result of
      Nothing -> do
                  hPutStrLn stderr $ "Could not parse JSON!"
                  return Nothing
      Just a -> return a

get :: (FromJSON a) => BS.ByteString -> IO (Maybe a)
get "" = return Nothing
get string = do
    let result = (Data.Aeson.decodeStrict string :: (FromJSON a) => Maybe a)
    case result of
      Nothing -> do
                  hPutStrLn stderr $ "Could not parse JSON!"
                  return Nothing
      Just a -> return a
      

fromFileL :: (FromJSON a) => FilePath -> IO (Maybe a)
fromFileL fName = BL.readFile fName `catch` exceptHandler >>= getL

fromFile :: (FromJSON a) => FilePath -> IO (Maybe a)
fromFile fName = BS.readFile fName `catch` exceptHandler >>= get

fromUrl :: (FromJSON a) => String -> [(C8.ByteString, String)] -> IO (Maybe a)
fromUrl url params = do
  request <- parseUrl url

  let request' = urlEncodedBody (map (second C8.pack) params) request

  (fmap responseBody . withManager . httpLbs $ request') `catch` urlExceptionHandler >>= getL

urlExceptionHandler :: SomeException -> IO (BL.ByteString)
urlExceptionHandler err = do
  hPutStrLn stderr $ show err
  return ""

exceptHandler :: (Data.String.IsString a) => SomeException -> IO a
exceptHandler err = do
  hPutStrLn stderr $ show err
  return ""
