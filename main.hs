import OAuth2
import Text.Printf
import Token
import File

import Control.Concurrent
import qualified Control.Exception as E
import System.Exit
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

mainLoop :: IO ()
mainLoop = do
  threadDelay 30000000
  putStrLn "Looping back"
  mainLoop

handler :: Int -> ThreadId -> IO ()
handler sig tid = do
  E.throwTo tid ExitSuccess
  putStrLn "Goodbye"

main :: IO ()
main = do
  webFlow <- createFlow "configuration" "authorization.txt"
  accessToken <- getTokens webFlow

  files <- getFileList accessToken webFlow

  printFiles files
  tid <- myThreadId
  installHandler sigINT (Catch $ handler 0 tid) Nothing
  mainLoop
