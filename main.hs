import OAuth2
import CSRFToken
import qualified Data.Map as M (lookup)
import Data.Maybe
import ConfigFile

main :: IO ()
main = do
  conf <- readConfig "configuration"

  let oauthScope = ["https://www.googleapis.com/auth/drive"]

  let redirectUri = "urn:ietf:wg:oauth:2.0:oob"

  let webFlow = OAuth2WebServerFlow (CSRFToken clientId "someState" "drive")
                clientSecret oauthScope redirectUri
                userAgent authUri tokenUri revokeUri loginHint deviceUri
                    where
                      userAgent = "something"
                      authUri = "https://accounts.google.com/o/oauth2/auth"
                      deviceUri = "https://accounts.google.com/o/oauth2/device/code"
                      revokeUri = "https://accounts.google.com/o/oauth2/revoke"
                      tokenUri = "https://accounts.google.com/o/oauth2/token"
                      loginHint = "None"
                      clientId = fromJust $ M.lookup "clientId" conf
                      clientSecret = fromJust $ M.lookup "clientSecret" conf

  putStrLn $ show webFlow
  putStrLn $ get webFlow
