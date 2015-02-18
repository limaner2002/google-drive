{-# LANGUAGE OverloadedStrings #-}
module OAuth2
    where

import URI
import CSRFToken
import Data.Monoid ((<>))
import Data.List (intercalate)
import Network.HTTP.Base (urlEncode)
import Token
import Text.Printf
import System.IO (hFlush, stdout)
import Network.HTTP.Conduit -- the main module
import Control.Arrow (second)
import ConfigFile

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL


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



