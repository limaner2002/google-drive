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
    { files :: [File]
    }

instance Show FileList
    where
      show (FileList []) = ""
      show (FileList (f:fs)) = show f++"\n"++show (FileList fs)

instance FromJSON FileList
    where
      parseJSON (Object o) = do
        files <- parseJSON =<< (o .: "items")
        return $ FileList files

printTable :: [[String]] -> IO ()
printTable rows = printBox $ hsep 2 left (map (vcat left . map text) (transpose rows))

printFiles_ :: FileList -> [[String]]
printFiles_ (FileList []) = []
printFiles_ (FileList (f:fs)) = [name f, mimeType f]:printFiles_ (FileList fs)

printFiles :: Maybe FileList -> IO ()
printFiles Nothing = putStrLn "Nothing"
printFiles (Just fl) = printTable $ printFiles_ fl

getFileList :: Maybe Token -> OAuth2WebServerFlow -> IO (Maybe FileList)
getFileList Nothing _ = return Nothing
getFileList (Just token) webFlow = do
  request <- parseUrl "https://www.googleapis.com/drive/v2/files"
  (files, status) <- fromRequest $ authorize (Just token) request
                     
  if statusCode status == 401
  then do
      putStrLn "Refreshing token now"
      newToken <- refreshTokens webFlow (Just token)
      save newToken
      (files', status') <- fromRequest $ authorize newToken request
      return files'
  else
      return files

 where
   authorize (Just token) request = request
                             {
                               requestHeaders = [(hAuthorization, B8.pack $ "Bearer " ++ accessToken token)]
                             }

