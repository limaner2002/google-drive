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

getInformation :: Maybe Token -> OAuth2WebServerFlow -> IO (Maybe About)
getInformation Nothing _ = return Nothing
getInformation (Just token) webFlow = do
  (result, status) <- fromAuthorizedUrl flowManager url [(hAuthorization, C8.pack $ "Bearer " ++ accessToken token)]
  return result
 where
   url = "https://www.googleapis.com/drive/v2/about"
   flowManager = getManager webFlow