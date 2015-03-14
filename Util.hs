{-# LANGUAGE OverloadedStrings #-}
module Util (
       fromFileL,
       fromFile,
       fromUrl,
       fromAuthorizedUrl,
       fromRequest,
       fromUrl',
       checkDirectory,
       downloadFile
    )
    where

import Data.Aeson
import Data.String
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import System.IO
import System.Directory
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import Control.Arrow (second)
import Network.HTTP.Conduit
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import Control.Monad.Trans.Resource (runResourceT)
import Network.HTTP.Types (HeaderName, hAuthorization)
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
		  BL.writeFile "/tmp/log.json" string
                  return Nothing
      Just a -> return a

get :: (FromJSON a) => BS.ByteString -> IO (Maybe a)
get "" = return Nothing
get string = do
    let result = (Data.Aeson.decodeStrict string :: (FromJSON a) => Maybe a)
    case result of
      Nothing -> do
                  hPutStrLn stderr $ "Could not parse JSON!"
		  BS.writeFile "/tmp/log.json" string
                  return Nothing
      Just a -> return a
      
-- Reads and decodes a JSON object from a file lazily.
fromFileL :: (FromJSON a) => FilePath -> IO (Maybe a)
fromFileL fName = BL.readFile fName `catch` exceptHandler >>= getL

-- Reads and decodes a JSON object from a file.
fromFile :: (FromJSON a) => FilePath -> IO (Maybe a)
fromFile fName = BS.readFile fName `catch` exceptHandler >>= get

-- Reads and decodes a JSON object from a web url.
fromUrl :: (FromJSON a) => Manager -> String -> [(C8.ByteString, String)] -> IO (Maybe a, Status)
fromUrl manager url params = do
  request <- parseUrl url

  fromRequest manager $ urlEncodedBody (map (second C8.pack) params) request

fromAuthorizedUrl :: (FromJSON a) => Manager -> String -> [(HeaderName, C8.ByteString)] -> IO (Maybe a, Status)
fromAuthorizedUrl manager url headers = do
  request <- parseUrl url

  fromRequest manager $ request { requestHeaders = headers }

fromRequest :: (FromJSON a) => Manager -> Request -> IO (Maybe a, Status)
fromRequest manager request = do
  (fmap (\x -> (responseBody x, responseStatus x)) $ httpLbs request manager)
  `catch` urlExceptionHandler
              >>= (\(json, status) -> do
                                      object <- getL json
                                      return (object, status))
  
urlExceptionHandler :: HttpException -> IO (BL.ByteString, Status)
urlExceptionHandler (StatusCodeException status _ _) = do
  hPutStrLn stderr $ "Error when "++show (statusCode status)++" fetching JSON from url"
  hPutStrLn stderr $ show $ statusMessage status
  return ("", status)
urlExceptionHandler someException = do
  error $ show someException

exceptHandler :: (Data.String.IsString a) => SomeException -> IO a
exceptHandler err = do
  hPutStrLn stderr "Error when reading JSON file"
  hPutStrLn stderr $ show err
  return ""

-- Reads and decodes a JSON object from a web url.
fromUrl' :: Manager -> String -> [(C8.ByteString, String)] -> IO (Maybe Token, Status)
fromUrl' manager url params = do
  request <- parseUrl url
  (response, status) <- getResponse manager $ urlEncodedBody (map (second C8.pack) params) request
  tok <- decodeToken (Data.Aeson.decode $ response)
  return (tok, status)

getResponse :: Manager -> Request -> IO (BL.ByteString, Status)
getResponse manager request =
  (fmap (\x -> (responseBody x, responseStatus x)) $ httpLbs request manager)
  `catch` urlExceptionHandler

tokenUrl :: BL.ByteString -> IO (Maybe Token)
tokenUrl body = decodeToken (Data.Aeson.decode body)

-- Checks to see if the directory specified in path exists and creates
-- it if it does not already exist.
checkDirectory :: FilePath -> IO()
checkDirectory path = do
  exists <- doesDirectoryExist path
  if exists == False
  then createDirectory path
  else return ()

downloadFile :: Manager -> Maybe String -> FilePath -> Token -> IO ()
downloadFile _ Nothing _ _ = return ()
downloadFile manager (Just url) localPath token = do
  putStrLn $ "Downloading file " ++ (show url) ++ " to " ++ localPath
  runResourceT $ do
    request <- liftIO $ parseUrl url
    result <- http (authorize request token) manager
    responseBody result $$+- sinkFile localPath

  putStrLn "Downloading file now."
 where
   authorize request token = request { requestHeaders = [(hAuthorization, C8.pack $ "Bearer " ++ accessToken token)] }