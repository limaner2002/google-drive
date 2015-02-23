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
      
-- Reads and decodes a JSON object from a file lazily.
fromFileL :: (FromJSON a) => FilePath -> IO (Maybe a)
fromFileL fName = BL.readFile fName `catch` exceptHandler >>= getL

-- Reads and decodes a JSON object from a file.
fromFile :: (FromJSON a) => FilePath -> IO (Maybe a)
fromFile fName = BS.readFile fName `catch` exceptHandler >>= get

-- Reads and decodes a JSON object from a web url.
fromUrl :: (FromJSON a) => String -> [(C8.ByteString, String)] -> IO (Maybe a)
fromUrl url params = do
  request <- parseUrl url

  fromRequest $ urlEncodedBody (map (second C8.pack) params) request

fromRequest :: (FromJSON a) => Request -> IO (Maybe a)
fromRequest request = (fmap responseBody . withManager . httpLbs $ request) `catch` urlExceptionHandler >>= getL

urlExceptionHandler :: HttpException -> IO (BL.ByteString)
urlExceptionHandler err = do
  hPutStrLn stderr "Error when fetching JSON"
  hPutStrLn stderr $ show err
  return ""

exceptHandler :: (Data.String.IsString a) => SomeException -> IO a
exceptHandler err = do
  hPutStrLn stderr "Error when reading JSON"
  hPutStrLn stderr $ show err
  return ""
