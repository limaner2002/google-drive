{-# LANGUAGE OverloadedStrings #-}

import OAuth2
import CSRFToken
import ConfigFile
import Text.Printf
import Token

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

import Network.HTTP.Conduit -- the main module
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  conf <- readConfig "configuration"
  authorization <- readConfig "authorization.txt"

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

  let webFlow = OAuth2WebServerFlow (CSRFToken clientId "someState" "drive")
                clientSecret oauthScope redirectUri
                userAgent authUri tokenUri revokeUri loginHint deviceUri


  printf "\nVisit the following URL to retreive a verification code:\n\n"
  printf "%s\n\n" $ getAuthorizeUrl webFlow
  printf "Verification code: "
  hFlush stdout
  authCode <- getLine

  let params = [("grant_type", "authorization_code"),
                ("client_id", C8.pack clientId),
                ("client_secret", C8.pack clientSecret),
                ("redirect_uri", C8.pack redirectUri),
                ("code", C8.pack authCode)
               ]

  printf "The authorization code is %s\n" authCode

  req <- parseUrl authUri
  let req2 = urlEncodedBody params req

  let j = "{  \"access_token\":\"1/fFAGRNJru1FTz70BzhT3Zg\",  \"expires_in\":3920,  \"token_type\":\"Bearer\",  \"refresh_token\":\"1/xEoDL4iW3cxlI7yDbSRFYNG01kVKM2C-259HOF2aQbI\"}"
  
  putStrLn $ show req2

  result <- withManager $ httpLbs $ req2
  putStrLn $ show (decode j)
  putStrLn $ show $ responseBody result
