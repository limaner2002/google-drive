{-# LANGUAGE OverloadedStrings #-}
module File
    ( File,
      FileList,
      printFiles,
      getFileList
    )
    where

import Data.Aeson
import Control.Applicative ((<$>),(<*>))
import Data.Maybe
import Control.Monad (mzero, void)
import qualified Data.ByteString.Lazy as BL
import Text.PrettyPrint.Boxes
import Data.List
import Token
import OAuth2
import Util
import qualified Data.ByteString.Char8 as B8
import Network.HTTP.Conduit
import Network.HTTP.Types (hAuthorization)
import Network.HTTP.Types.Status (Status(..))
import Data.Monoid

data File = File
          { name :: String,
            kind :: String,
            mimeType :: String
          }

instance Show File
    where
      show file = "name: " ++ name file ++ " mimeType: " ++ mimeType file

instance FromJSON File
    where
      parseJSON (Object o) = File <$> o .: "title" <*>
                             o .: "kind" <*>
                             o .: "mimeType"
      parseJSON _ = mzero

data FileList = FileList
    { files :: [File],
      nextPage :: Maybe PageToken
    }

instance Show FileList
    where
      show (FileList [] _) = ""
      show (FileList (f:fs) _) = show f++"\n"++show (FileList fs Nothing)

instance FromJSON FileList
    where
      parseJSON (Object o) = FileList <$>
                             o .: "items" <*>
                             o .:? "nextPageToken"
        -- do
        -- files <- parseJSON =<< (o .: "items")
        -- nextPage <- parseJSON =<< (o .:? "nextPageToken")
        -- return $ FileList files nextPage

instance Monoid FileList where
    mempty = FileList [] Nothing
    mappend (FileList l _) (FileList r _) = FileList (l ++ r) Nothing

printTable :: [[String]] -> IO ()
printTable rows = printBox $ hsep 2 left (map (vcat left . map text) (transpose rows))

printFiles_ :: FileList -> [[String]]
printFiles_ (FileList [] _) = []
printFiles_ (FileList (f:fs) next) = [name f, mimeType f]:printFiles_ (FileList fs next)

printFiles :: Maybe FileList -> IO ()
printFiles Nothing = putStrLn "Nothing"
printFiles (Just fl) = printTable $ printFiles_ fl

getNextPages :: Maybe Token -> Maybe PageToken -> String -> IO (Maybe FileList)
getNextPages _ Nothing _ = return (Nothing)
getNextPages Nothing _ _ = return (Nothing)
getNextPages (Just token) (Just nextPageToken) url = do
  request <- parseUrl (url++"?pageToken="++nextPageToken)
  (files, _) <- fromRequest $ authorize token request
  next <- getNextPages (Just token) (files >>= nextPage) url
  return files
 where
   authorize token request = request
                             {
                               requestHeaders = [(hAuthorization, B8.pack $ "Bearer " ++ accessToken token)]
                             }

getFileList :: Maybe Token -> OAuth2WebServerFlow -> IO (Maybe FileList)
getFileList Nothing _ = return Nothing
getFileList token webFlow = do
  request <- parseUrl url
  (files, status) <- fromRequest $ authorize token request
                     
  if statusCode status == 401
  then do
      putStrLn "Refreshing token now"
      newToken <- refreshTokens webFlow token
      -- Change this so that it doesn't try saving the refresh token
      -- every time.
      save newToken
      (files', _) <- fromRequest $ authorize newToken request
      next <- getNextPages newToken (files' >>= nextPage) url
      return (files' `mappend` next)
  else do
      -- (next, status') <- getNextPages token (files >>= nextPage) url
      getNextPages token (files >>= nextPage) url >>= (\x -> return $ files `mappend` x)


 where
   authorize (Just token) request = request
                             {
                               requestHeaders = [(hAuthorization, B8.pack $ "Bearer " ++ accessToken token)]
                             }
   url = "https://www.googleapis.com/drive/v2/files"
