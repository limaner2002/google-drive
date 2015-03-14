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
      listChanges,
      processChange
    )
    where

import Data.Maybe (isJust)
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
     { changeKind :: String,
       file :: Maybe File,
       modificationDate :: String,
       deleted :: Bool
     } deriving (Show)

instance FromJSON ChangeItem
    where
      parseJSON (Object o) = ChangeItem <$> o .: "kind" <*>
      			     		    o .:? "file" <*>
      			     		    o .: "modificationDate" <*>
					    o .: "deleted"
      parseJSON _ = mzero

data Change = Change
    { items :: [ChangeItem],
      largestChangeId :: String,
      nextPageToken :: Maybe PageToken
    } deriving (Show)

instance FromJSON Change
    where
      parseJSON (Object o) = Change <$> o .:? "items" .!= [] <*>
      			     	    	o .: "largestChangeId" <*>
					o .:? "nextPageToken"
      parseJSON _ = mzero

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
	    lastModifyingUser :: User,
	    downloadUrl :: Maybe String
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
				   obj .: "lastModifyingUser" <*>
				   obj .:? "downloadUrl") value

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

processChange :: Change -> Flow ()
processChange change= do
  webFlow <- get
  let flowManager = getManager webFlow
  token <- liftIO $ getAuthToken webFlow
  let baseDir = localDirectory webFlow

  liftIO $ checkDirectory $ localDirectory webFlow;

  liftIO $ mapM_ (\item -> do
  	   	     case (file item) of
		     	  Nothing -> return ()
		     	  Just f -> do
  	   	     	       if (deleted item) == True
		     	       then putStrLn $ name f ++ " was deleted"
			       else downloadFile flowManager (downloadUrl f) (baseDir ++ "/" ++ name f) (token)
		 ) (items change)
  liftIO $ putStrLn $ show change
