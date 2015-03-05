{-# LANGUAGE OverloadedStrings #-}
module File
    ( Resource(..),
      File(..),
      FileList(..),
      Parent(..),
      printFiles,
      getFileList,
      getChange
    )
    where

import Data.Aeson
import Data.Aeson.Types
import Control.Applicative ((<$>),(<*>))
import Data.Maybe
import Control.Monad (mzero, void)
import qualified Data.ByteString.Lazy as BL
import Text.PrettyPrint.Boxes
import qualified Data.ByteString.Char8 as B8
import Network.HTTP.Conduit
import Network.HTTP.Types (hAuthorization)
import Network.HTTP.Types.Status (Status(..))
import Data.Monoid
import Data.List

import Token
import OAuth2
import Util
import Resource

data ChangeItem = ChangeItem
     { file :: File
     }

instance FromJSON ChangeItem
    where
      parseJSON (Object o) = ChangeItem <$> o.: "file"
      parseJSON _ = mzero

data Change = Change
    { items :: [ChangeItem]
    }

instance FromJSON Change
    where
      parseJSON (Object o) = Change <$> o .: "items"
      parseJSON _ = mzero

instance Show Change
    where
      show change = foldl (++) "" changeMessages
      	   where
	     changeMessages = map (\x -> itemName x ++ " was changed by " ++ modifiedBy x ++ "\n") $ items change
	     itemName = name . file
	     modifiedBy = displayName . lastModifyingUser . file

data File = File
          { fileResource :: Resource,
            name :: String,
            mimeType :: String,
            parents :: [Parent],
	    lastModifyingUser :: User
          }

instance Show File
    where
      show file = "name: " ++ name file ++ " mimeType: " ++ mimeType file

instance FromJSON File
    where
      parseJSON value = do
        withObject "File" (\obj -> File <$> (parseJSON value :: Parser Resource) <*>
                                   obj .: "title" <*>
                                   obj .: "mimeType" <*>
                                   obj .:? "parents" .!= [] <*>
				   obj .: "lastModifyingUser") value

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

instance Monoid FileList where
    mempty = FileList [] Nothing
    mappend (FileList l _) (FileList r _) = FileList (l ++ r) Nothing

printTable :: [[String]] -> IO ()
printTable rows = printBox $ hsep 2 left (map (vcat left . map text) (transpose rows))

printFiles_ :: FileList -> [[String]]
printFiles_ (FileList [] _) = []
printFiles_ (FileList (f:fs) next) = [name f, mimeType f, show $ map isRoot (parents f)]:printFiles_ (FileList fs next)

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
  return $ files `mappend` next
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
      getNextPages token (files >>= nextPage) url >>= (\x -> return $ files `mappend` x)


 where
   authorize (Just token) request = request
                             {
                               requestHeaders = [(hAuthorization, B8.pack $ "Bearer " ++ accessToken token)]
                             }
   url = "https://www.googleapis.com/drive/v2/files"

getChange :: Maybe Token -> OAuth2WebServerFlow -> Int -> IO (Maybe Change)
getChange Nothing _ _ = return Nothing
getChange (Just token) webFlow changeId = do
  (result, status) <- fromAuthorizedUrl url [(hAuthorization, B8.pack $ "Bearer " ++ accessToken token)]
  return result
 where
  url = "https://www.googleapis.com/drive/v2/changes?pageToken=" ++ (show changeId)