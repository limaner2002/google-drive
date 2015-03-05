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
import Network.HTTP.Types (HeaderName)
import Network.HTTP.Types.Status (Status(..))
import qualified Data.ByteString.Char8 as C8

-- Maybe can remove this and make tokenUrl more general?
import Token 

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

fromAuthorizedUrl :: (FromJSON a) => String -> [(HeaderName, C8.ByteString)] -> IO (Maybe a, Status)
fromAuthorizedUrl url headers = do
  request <- parseUrl url

  fromRequest $ request { requestHeaders = headers }

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

-- Reads and decodes a JSON object from a web url.
fromUrl' :: String -> [(C8.ByteString, String)] -> IO (Maybe Token, Status)
fromUrl' url params = do
  request <- parseUrl url
  (response, status) <- getResponse $ urlEncodedBody (map (second C8.pack) params) request
  tok <- decodeToken (Data.Aeson.decode $ response)
  return (tok, status)

getResponse :: Request -> IO (BL.ByteString, Status)
getResponse request =
  (fmap (\x -> (responseBody x, responseStatus x)) . withManager . httpLbs $ request)
  `catch` urlExceptionHandler

tokenUrl :: BL.ByteString -> IO (Maybe Token)
tokenUrl body = decodeToken (Data.Aeson.decode body)
