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
import Network.HTTP.Types.Status (Status(..))
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
fromUrl :: (FromJSON a) => String -> [(C8.ByteString, String)] -> IO (Maybe a, Status)
fromUrl url params = do
  request <- parseUrl url

  fromRequest $ urlEncodedBody (map (second C8.pack) params) request

fromRequest :: (FromJSON a) => Request -> IO (Maybe a, Status)
fromRequest request = do
  (fmap (\x -> (responseBody x, responseStatus x)) . withManager . httpLbs $ request)
  `catch` urlExceptionHandler
              >>= (\(json, status) -> do
                                      object <- getL json
                                      return (object, status))

urlExceptionHandler :: HttpException -> IO (BL.ByteString, Status)
urlExceptionHandler (StatusCodeException status _ _) = do
  hPutStrLn stderr $ "Error when "++show (statusCode status)++" fetching JSON from url"
  hPutStrLn stderr $ show $ statusMessage status
  return ("", status)

exceptHandler :: (Data.String.IsString a) => SomeException -> IO a
exceptHandler err = do
  hPutStrLn stderr "Error when reading JSON file"
  hPutStrLn stderr $ show err
  return ""
