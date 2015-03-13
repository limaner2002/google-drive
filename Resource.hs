{-# LANGUAGE OverloadedStrings #-}
module Resource
    where
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero, void)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types (hAuthorization)
import Control.Monad.State
import Control.Monad.Except

import Token
import OAuth2
import Util

data Resource = Resource {
      kind :: String,
      id :: String,
      selfLink :: String
    } deriving Show

instance FromJSON Resource
    where
      parseJSON (Object o) = Resource <$> o .: "kind" <*>
                             o .: "id" <*>
                             o .: "selfLink"
      parseJSON _ = mzero


data Parent = Parent
            { parentResource :: Resource,
              isRoot :: Bool
            } deriving Show

instance FromJSON Parent
    where
      parseJSON value = do
        withObject "Parent" (\obj -> do
         			     let isRoot = obj .: "isRoot"
                                     Parent <$> (parseJSON value :: Parser Resource) <*> isRoot) value
data About = About
    { userName :: String,
      user :: User,
      quotaBytesTotal :: String,
      quotaBytesUsed :: String,
      quotaBytesUsedAggregate :: String,
      quotaBytesUsedInTrash :: String,
      quotaType :: String,
      largestChange :: String
    }

instance Show About
    where
      show info = "Signed in as: "++(userName info)
      

instance FromJSON About
    where
      parseJSON value = do
      	withObject "User" (\obj -> About <$> obj .: "name" <*>
				   -- (parseJSON value :: Parser User) <*>
				   obj .: "user" <*>
				   obj .: "quotaBytesTotal" <*>
				   obj .: "quotaBytesUsed" <*>
				   obj .: "quotaBytesUsedAggregate" <*>
				   obj .: "quotaBytesUsedInTrash" <*>
				   obj .: "quotaType" <*>
				   obj .: "largestChangeId") value

data User = User
    { displayName :: String,
      isAuthenticatedUser :: Bool,
      permissionId :: String,
      emailAddress :: String
    }

instance FromJSON User
    where
      parseJSON (Object o) = User <$> o .: "displayName" <*>
                             o .: "isAuthenticatedUser" <*>
                             o .: "permissionId" <*>
                             o .: "emailAddress"
      parseJSON _ = mzero

getInformation :: Flow About
getInformation = do
  webFlow <- get
  let flowManager = getManager webFlow
  let url = "https://www.googleapis.com/drive/v2/about"
  tok <- liftIO $ getAuthToken webFlow

  (result, status) <- liftIO $ fromAuthorizedUrl flowManager url [(hAuthorization, C8.pack $ "Bearer " ++ accessToken tok)]
  case result of
       Nothing -> throwError $ "Could not get drive information.\nAccess Token: " ++ show tok
       Just about -> return about
