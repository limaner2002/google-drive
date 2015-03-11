{-# LANGUAGE OverloadedStrings #-}
module OAuth2 (
               OAuth2WebServerFlow,
               createFlow,
	       endFlow,
               getTokens,
	       checkToken,
               refreshTokens,
	       getManager
              )
    where

import URI
import CSRFToken
import Data.Aeson
import Data.Maybe
import Data.Monoid ((<>))
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Types (hAuthorization)
import Network.HTTP.Types.Status (Status(..))
import Network.HTTP.Conduit
import Token
import Text.Printf
import System.IO (hFlush, stdout)
import Network.HTTP.Conduit -- the main module
import ConfigFile
import Control.Exception
import Util
-- import Network.HTTP.Client (defaultManagerSettings)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as C8

data OAuth2WebServerFlow = OAuth2WebServerFlow
    { token :: !CSRFToken,
      clientSecret :: !String,
      scope :: ![String],
      redirectUri :: !String,
      userAgent :: !String,
      authUri :: !String,
      tokenUri :: !String,
      revokeUri :: !String,
      loginHint :: !String,
      deviceUri :: !String,
      accessToken :: Maybe Token,
      manager :: Manager
    }

createFlow :: String -> String -> IO (OAuth2WebServerFlow)
createFlow configFile authorizationFile = do
  manager <- newManager conduitManagerSettings
  conf <- readConfig configFile
  authorization <- readConfig authorizationFile

  let oauthScope = [param "scopes" conf]
  let redirectUri = param "redirectUri" conf
  let userAgent = param "userAgent" conf
  let authUri = param "authUri" conf
  let deviceUri = param "deviceUri" conf
  let revokeUri = param "revokeUri" conf
  let tokenUri = param "tokenUri" conf
  let loginHint = param "loginHint" conf
  let clientId = param "clientId" conf
  let clientSecret = param "clientSecret" conf

  return $ OAuth2WebServerFlow (CSRFToken clientId "someState" "drive")
         clientSecret oauthScope redirectUri
         userAgent authUri tokenUri revokeUri loginHint deviceUri Nothing manager

endFlow :: OAuth2WebServerFlow -> IO ()
endFlow = closeManager . manager
  
getManager :: OAuth2WebServerFlow -> Manager
getManager = manager

getAuthorizeUrl :: OAuth2WebServerFlow -> String
getAuthorizeUrl flow = request flow

getTokens :: OAuth2WebServerFlow -> IO (Maybe Token)
getTokens flow = do
  tok <- fromFile "token"
  checkToken flow tok

checkToken :: OAuth2WebServerFlow -> Maybe Token -> IO (Maybe Token)
checkToken flow Nothing = do
  putStrLn "Requesting new tokens"
  requestTokens flow
checkToken flow (Just token) = do
  currentTime <- getPOSIXTime
  if expires token > (realToFrac currentTime :: Double)
  then return $ Just token
  else do
    putStrLn "Token has expired. Requesting a new one"
    newToken <- refreshTokens flow (Just token)
    save newToken
    return newToken
  

refreshTokens :: OAuth2WebServerFlow -> Maybe Token -> IO (Maybe Token)
refreshTokens _ Nothing = return Nothing
refreshTokens flow (Just oldToken) = do
  refreshToken <- fromKeychain "My Google Drive" "MyDrive"
  putStrLn "Refresh token is"

  let tok = token flow
  let params = [("client_id", clientId tok),
                ("client_secret", clientSecret flow),
                ("grant_type", "refresh_token"),
                ("refresh_token", fromJust $ refreshToken)
               ]

  fromUrl' flowManager (tokenUri flow) params >>= (\newToken -> return $ fst newToken)
    where
      flowManager = getManager flow

passRefreshToken :: Maybe Token -> Maybe String -> IO (Maybe Token)
passRefreshToken Nothing _ = return Nothing
passRefreshToken _ Nothing = return Nothing
passRefreshToken (Just newToken) refreshToken = do
  let result = Just $ newToken { refreshToken = refreshToken}
  return result

requestTokens :: OAuth2WebServerFlow -> IO (Maybe Token)
requestTokens flow = do
  let tok = token flow

  printf "\nVisit the following URL to retreive a verification code:\n\n"
  printf "%s\n\n" $ getAuthorizeUrl flow
  printf "Verification code: "
  hFlush stdout
  authCode <- getLine

  let params = [("client_id", clientId tok),
                ("client_secret", clientSecret flow),
                ("grant_type", "authorization_code"),
                ("redirect_uri", redirectUri flow),
                ("code", authCode)
               ]

  result <- fromUrl' flowManager (tokenUri flow) params
  save $ fst result
  return $ fst result
 where
   flowManager = getManager flow

instance Show OAuth2WebServerFlow
    where
      show oauth = show $ clientId $ token oauth

instance URI OAuth2WebServerFlow
    where
      render flow = authUri flow
                    <>"?scope=" <> intercalate "+" (map urlEncode (scope flow))
                    <>"&redirect_uri="<>redirectUri flow
                    <>"&response_type=code"
                    <>"&client_id="<>clientId (token flow)

authorizedRequest :: (FromJSON a) => OAuth2WebServerFlow -> String -> IO (Maybe a, Status)
authorizedRequest flow url = do
  request <- parseUrl url

  validToken <- checkToken $ OAuth2.accessToken flow

  fromRequest flowManager $ request { requestHeaders = headers validToken }
 where
   flowManager = manager flow
   headers token = [(hAuthorization, C8.pack $ "Bearer " ++ (fromMaybe "" tokenString))]
   tokenString = fmap (OAuth2.accessToken flow) Token.accessToken