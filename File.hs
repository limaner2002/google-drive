{-# LANGUAGE OverloadedStrings #-}
module File
    ( Resource(..),
      File(..),
      FileList(..),
      Parent(..),
      printFiles,
      listFiles,
      getChange,
      Change(..),
      listChanges
    )
    where

import Data.Aeson
import Data.Aeson.Types
import Control.Applicative ((<$>),(<*>))
import Control.Arrow (second)
-- import Data.Maybe
import Control.Monad (mzero, void)
import qualified Data.ByteString.Lazy as BL
import Text.PrettyPrint.Boxes
import qualified Data.ByteString.Char8 as B8
import Network.HTTP.Conduit
import Network.HTTP.Types (hAuthorization)
import Network.HTTP.Types.Status (Status(..))
import Data.Monoid
import Data.List
import Control.Monad.Except
import Control.Monad.State

import Token
import OAuth2
import Util
import Resource

data ChangeItem = ChangeItem
     { file :: Maybe File,
       modificationDate :: String
     }

instance FromJSON ChangeItem
    where
      parseJSON (Object o) = ChangeItem <$> o .:? "file" <*>
      			     		    o .: "modificationDate"
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
	     changeMessages = map (\x -> itemName x ++ " was changed by " ++ modifiedBy x ++ " at " ++ time x ++ " \n") $ items change
	     itemName item = show $ fmap (name) $ file item
	     modifiedBy item = show $ fmap (displayName . lastModifyingUser) $ file item
	     time = modificationDate

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

listFiles :: Maybe PageToken -> Flow FileList
listFiles Nothing = return $ FileList [] Nothing
listFiles (Just []) = do
  files <- getFileList "https://www.googleapis.com/drive/v2/files"
  next <- listFiles (nextPage files)
  return $ files `mappend` next

listFiles (Just nextPageToken) = do
  files <- getFileList ("https://www.googleapis.com/drive/v2/files?pageToken="++nextPageToken)
  next <- listFiles (nextPage files)
  return $ files `mappend` next
--  where
--    authorize token request = request
--                              {
--                                requestHeaders = [(hAuthorization, B8.pack $ "Bearer " ++ accessToken token)]
--                              }


getFileList :: String -> Flow FileList
getFileList url = do
  webFlow <- get
  let flowManager = getManager webFlow
  token <- liftIO $ getAuthToken webFlow

  request <- parseUrl url
  (files, status) <- liftIO $ fromRequest flowManager $ authorize token request

  case files of
       Nothing -> throwError "Could not get list of files!"
       Just list -> return list
--   getNextPages webFlow token (files >>= nextPage) url >>= (\x -> return $ files `mappend` x)

 where
   authorize token request = request
                             {
                               requestHeaders = [(hAuthorization, B8.pack $ "Bearer " ++ accessToken token)]
                             }

getChange :: Int -> Flow Change
getChange changeId = do
  webFlow <- get
  let flowManager = getManager webFlow
  token <- liftIO $ getAuthToken webFlow
  (result, status) <- liftIO $ fromAuthorizedUrl flowManager url [(hAuthorization, B8.pack $ "Bearer " ++ (accessToken token))]
  case result of
       Nothing -> throwError "Could not get change information!"
       Just chg -> return chg
 where
  url = "https://www.googleapis.com/drive/v2/changes?pageToken=" ++ (show changeId)

listChanges :: Int -> Maybe PageToken -> Flow Change
listChanges changeId Nothing = getChange changeId
listChanges changeId (Just nextChangePageToken) = do
  request <- parseUrl url
  webFlow <- get
  token <- liftIO $ getAuthToken webFlow
  let flowManager = getManager webFlow

  (change, status) <- liftIO $ fromRequest flowManager $ authorize (accessToken token) request
  case change of
       Nothing -> throwError "Could not get list of changes!"
       Just chg -> do
       	    next <- listChanges changeId (change >>= nextPageToken)
  	    return $ chg `mappend` next
 where
  url = "https://www.googleapis.com/drive/v2/changes"
  headers token = [(hAuthorization, B8.pack $ "Bearer " ++ show token)]
  params = [("pageToken", nextChangePageToken),
       	    ("startChangeId", show changeId)]
  authorize token request =
   	    urlEncodedBody (map (second B8.pack) params)$ request 
	     		    	 	    	     	   {
								requestHeaders = headers token
	     						   }
