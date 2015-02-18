import OAuth2
import Text.Printf

main :: IO ()
main = do
  webFlow <- createFlow "configuration" "authorization.txt"
  accessToken <- getTokens webFlow
  putStrLn $ show accessToken
