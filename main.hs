import OAuth2
import Text.Printf
import Token

import Network.HTTP.Conduit
import Network.HTTP.Types (hAuthorization)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
  webFlow <- createFlow "configuration" "authorization.txt"
  accessToken <- getTokens webFlow
  save "token" accessToken

  request <- parseUrl "https://www.googleapis.com/drive/v2/files"
  response <- withManager $ httpLbs $ authorize accessToken request
  putStrLn $ show $ authorize accessToken request

  L8.putStrLn $ responseBody response

 where
   authorize token request = request
                             {
                               requestHeaders = [(hAuthorization, B8.pack $ "Bearer " ++ accessToken token)]
                             }
