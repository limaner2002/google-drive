import OAuth2
import Text.Printf
import Token
import File

import Control.Concurrent
import qualified Control.Exception as E
import System.Exit
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import Data.Maybe

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

  fileList <- getFileList accessToken webFlow

  printFiles fileList
  -- putStrLn $ show $ filter (\x -> name x == "Math") (files (fromJust fileList))
  tid <- myThreadId
  installHandler sigINT (Catch $ handler 0 tid) Nothing
  mainLoop
