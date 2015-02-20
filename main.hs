import OAuth2
import Text.Printf
import Token
import File

import Network.HTTP.Conduit
import Network.HTTP.Types (hAuthorization)
import Network.HTTP.Types.Status (Status(..))
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Exception
import Data.Default (def)

checkStatus200 st@(Status sc _) rh cj = do
  if (200 <= sc && sc < 300) || sc == 404 || sc == 401
  then Nothing
  else (checkStatus def) st rh cj

main :: IO ()
main = do
  webFlow <- createFlow "configuration" "authorization.txt"
  accessToken <- getTokens webFlow
  save "token" accessToken

  request <- parseUrl "https://www.googleapis.com/drive/v2/files"
  let request' = request { checkStatus = checkStatus200 }
  response <- withManager $ httpLbs $ authorize accessToken request'
  let status = statusCode . responseStatus $ response
  if status == 401
  then do
      putStrLn "Refreshing token now"
      newToken <- refreshTokens webFlow accessToken
      save "token" newToken
  else
      putStrLn "Continuing as usual."

  L8.putStrLn $ responseBody response
  putStrLn "Done!"
 where
   authorize (Just token) request = request
                             {
                               requestHeaders = [(hAuthorization, B8.pack $ "Bearer " ++ accessToken token)]
                             }
