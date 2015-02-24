import OAuth2
import Text.Printf
import Token
import File

main :: IO ()
main = do
  webFlow <- createFlow "configuration" "authorization.txt"
  accessToken <- getTokens webFlow

  files <- getFileList accessToken webFlow

  printFiles files
  putStrLn "Done!"
