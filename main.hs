{-# LANGUAGE OverloadedStrings #-}

import OAuth2
import CSRFToken
import ConfigFile
import Text.Printf
import Token

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

import Network.HTTP.Conduit -- the main module
import Network.HTTP.Base (urlEncode)
import System.IO (hFlush, stdout)
import Control.Arrow (second)

main :: IO ()
main = do
  conf <- readConfig "configuration"
  -- authorization <- readConfig "authorization.txt"

  let oauthScope = map urlEncode [param "scopes" conf]
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

  let params = [("client_id", clientId),
                ("client_secret", clientSecret),
                ("grant_type", "authorization_code"),
                ("redirect_uri", redirectUri),
                ("code", authCode)
               ]

  printf "The authorization code is %s\n" authCode

  req <- parseUrl tokenUri
  let req2 = urlEncodedBody (map (second C8.pack) params) req

  -- let j = "{  \"access_token\":\"1/fFAGRNJru1FTz70BzhT3Zg\",  \"expires_in\":3920,  \"token_type\":\"Bearer\",  \"refresh_token\":\"1/xEoDL4iW3cxlI7yDbSRFYNG01kVKM2C-259HOF2aQbI\"}"
  
  putStrLn $ show req2

  result <- withManager $ httpLbs $ req2
  -- putStrLn $ show (decode j)
  putStrLn $ show $ responseBody result
