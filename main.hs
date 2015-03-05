import OAuth2
import Text.Printf
import Token
import File
import Resource

import Control.Concurrent
import qualified Control.Exception as E
import System.Exit
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import Data.Maybe

import Tree

mainLoop :: Int -> OAuth2WebServerFlow -> Maybe Token -> IO ()
mainLoop lastChange webFlow accessToken = do
  threadDelay 30000000
  putStrLn "Looping back"

  about <- getInformation accessToken webFlow
  case about of
       Nothing -> error "Could not get information!"
       (Just info) -> do

       	     	      let change = read (largestChangeId info) :: Int
		      if change > lastChange
		      then do

		      putStrLn "Something changed!"
		      mainLoop change webFlow accessToken

		      else mainLoop lastChange webFlow accessToken

handler :: Int -> ThreadId -> IO ()
handler sig tid = do
  E.throwTo tid ExitSuccess
  putStrLn "Goodbye"

checkInfo :: Maybe About -> About
checkInfo Nothing = error "Could not get information!"
checkInfo (Just info) = info

main :: IO ()
main = do
  webFlow <- createFlow "configuration" "authorization.txt"
  accessToken <- getTokens webFlow

  fileList <- getFileList accessToken webFlow
  createDirectory $ fromJust fileList

  about <- getInformation accessToken webFlow
  putStrLn $ show about

  tid <- myThreadId
  installHandler sigINT (Catch $ handler 0 tid) Nothing
  mainLoop (read $ largestChangeId $ checkInfo about) webFlow accessToken
