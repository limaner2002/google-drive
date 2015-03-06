{-# LANGUAGE OverloadedStrings #-}
module File
    ( Resource(..),
      File(..),
      FileList(..),
      Parent(..),
      printFiles,
      getFileList,
      getChange,
      Change(..),
      listChanges
    )
    where

import Data.Aeson
import Data.Aeson.Types
import Control.Applicative ((<$>),(<*>))
import Control.Arrow (second)
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
     { file :: Maybe File
     }

instance FromJSON ChangeItem
    where
      parseJSON (Object o) = ChangeItem <$> o .:? "file"
      parseJSON _ = mzero

data Change = Change
    { items :: [ChangeItem],
      largestChangeId :: String,
      nextPageToken :: Maybe PageToken
    }

instance FromJSON Change
    where
      parseJSON (Object o) = Change <$> o .:? "items" .!= [] <*>
      			     	    	o .: "largestChangeId" <*>
					o .:? "nextPageToken"
      parseJSON _ = mzero

instance Show Change
    where
      show change = foldl (++) "" $ changeMessages
      	   where
	     changeMessages = map (\x -> itemName x ++ " was changed by " ++ modifiedBy x ++ "\n") $ items change
	     itemName item = show $ fmap (name) $ file item
	     modifiedBy item = show $ fmap (displayName . lastModifyingUser) $ file item

instance Monoid Change where
     mempty = Change [] "" Nothing
     mappend (Change l lChangeId _) (Change r rChangeId _) = Change (l ++ r) (largest) Nothing
     	     where
		largest = show $ max lChange rChange
		lChange = read lChangeId :: Int
		rChange = read rChangeId :: Int

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

getNextPages :: OAuth2WebServerFlow -> Maybe Token -> Maybe PageToken -> String -> IO (Maybe FileList)
getNextPages _ _ Nothing _ = return (Nothing)
getNextPages _ Nothing _ _ = return (Nothing)
getNextPages webFlow (Just token) (Just nextPageToken) url = do
  request <- parseUrl (url++"?pageToken="++nextPageToken)
  (files, _) <- fromRequest flowManager $ authorize token request
  next <- getNextPages webFlow (Just token) (files >>= nextPage) url
  return $ files `mappend` next
 where
   authorize token request = request
                             {
                               requestHeaders = [(hAuthorization, B8.pack $ "Bearer " ++ accessToken token)]
                             }
   flowManager = getManager webFlow

getFileList :: Maybe Token -> OAuth2WebServerFlow -> IO (Maybe FileList)
getFileList Nothing _ = return Nothing
getFileList token webFlow = do
  request <- parseUrl url
  (files, status) <- fromRequest flowManager $ authorize token request
                     
  if statusCode status == 401
  then do
      putStrLn "Refreshing token now"
      newToken <- refreshTokens webFlow token
      -- Change this so that it doesn't try saving the refresh token
      -- every time.
      save newToken
      (files', _) <- fromRequest flowManager $ authorize newToken request
      next <- getNextPages webFlow newToken (files' >>= nextPage) url
      return (files' `mappend` next)
  else do
      getNextPages webFlow token (files >>= nextPage) url >>= (\x -> return $ files `mappend` x)


 where
   authorize (Just token) request = request
                             {
                               requestHeaders = [(hAuthorization, B8.pack $ "Bearer " ++ accessToken token)]
                             }
   url = "https://www.googleapis.com/drive/v2/files"
   flowManager = getManager webFlow

getChange :: Maybe Token -> OAuth2WebServerFlow -> Int -> IO (Maybe Change)
getChange Nothing _ _ = return Nothing
getChange (Just token) webFlow changeId = do
  (result, status) <- fromAuthorizedUrl flowManager url [(hAuthorization, B8.pack $ "Bearer " ++ accessToken token)]
  return result
 where
  url = "https://www.googleapis.com/drive/v2/changes?pageToken=" ++ (show changeId)
  flowManager = getManager webFlow

listChanges :: Maybe Token -> OAuth2WebServerFlow -> Int -> Maybe PageToken -> IO (Maybe Change)
listChanges Nothing _ _ _ = return Nothing
listChanges mToken webFlow changeId Nothing = getChange mToken webFlow changeId
listChanges (Just token) webFlow changeId (Just nextChangePageToken) = do
  request <- parseUrl url

  (change, status) <- fromRequest flowManager $ authorize token request
  next <- listChanges (Just token) webFlow changeId (change >>= nextPageToken)
  return $ change `mappend` next
 where
   flowManager = getManager webFlow
   url = "https://www.googleapis.com/drive/v2/changes"
   headers = [(hAuthorization, B8.pack $ "Bearer " ++ accessToken token)]
   params = [("pageToken", nextChangePageToken),
   	     ("startChangeId", show changeId)]
   authorize token request =
   	     urlEncodedBody (map (second B8.pack) params)$ request 
	     		    	 	    	     	   {
								requestHeaders = headers
	     						   }