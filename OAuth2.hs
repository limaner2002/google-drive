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
import Network.HTTP.Base (urlEncode)
import Token
import Text.Printf
import System.IO (hFlush, stdout)
import Network.HTTP.Conduit -- the main module
import Control.Arrow (second)
import ConfigFile
import Control.Exception
import Util

import qualified Data.ByteString.Char8 as C8
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

getTokens :: OAuth2WebServerFlow -> IO (Token)
getTokens flow = do
  tok <- fromFile "token"
  case tok of
    Nothing -> do
             putStrLn "Requesting new tokens"
             requestTokens flow
    Just token -> return token

refreshTokens :: OAuth2WebServerFlow -> Token -> IO (Token)
refreshTokens flow oldToken = do
  let tok = token flow
  let params = [("client_id", clientId tok),
                ("client_secret", clientSecret flow),
                ("grant_type", "refresh_token"),
                ("refresh_token", fromJust $ refreshToken oldToken)
               ]
  request <- parseUrl $ tokenUri flow
  result <- withManager $ httpLbs $ urlEncodedBody (map (second C8.pack) params) request

  let newToken = decode $ responseBody result

  return $ newToken { refreshToken = refreshToken oldToken }

requestTokens :: OAuth2WebServerFlow -> IO (Token)
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

  request <- parseUrl $ tokenUri flow

  result <- withManager $ httpLbs $ urlEncodedBody (map (second C8.pack) params) request

  return $ decode $ responseBody result

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



