{-# LANGUAGE OverloadedStrings #-}
module OAuth2 (
               OAuth2WebServerFlow,
               createFlow,
               getTokens,
               refreshTokens
              )
    where

import URI
import CSRFToken
import Data.Monoid ((<>))
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Base (urlEncode)
import Token
import Text.Printf
import System.IO (hFlush, stdout)
import Network.HTTP.Conduit -- the main module
import ConfigFile
import Control.Exception
import Util

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8


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
      deviceUri :: !String
    }

createFlow :: String -> String -> IO (OAuth2WebServerFlow)
createFlow configFile authorizationFile = do
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
         userAgent authUri tokenUri revokeUri loginHint deviceUri

getAuthorizeUrl :: OAuth2WebServerFlow -> String
getAuthorizeUrl flow = request flow

getTokens :: OAuth2WebServerFlow -> IO (Maybe Token)
getTokens flow = do
  tok <- fromFile "token"
  case tok of
    Nothing -> do
             putStrLn "Requesting new tokens"
             requestTokens flow
    Just token -> do
                   currentTime <- getPOSIXTime
                   if expires token > (realToFrac currentTime :: Double)
                   then return $ Just token
                   else do
                     putStrLn "Token has expired. Requesting a new one"
                     newToken <- refreshTokens flow tok
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

  fromUrl' (tokenUri flow) params >>= (\newToken -> return $ fst newToken)

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

  result <- fromUrl' (tokenUri flow) params
  save $ fst result
  return $ fst result

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



