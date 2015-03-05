import OAuth2
import Text.Printf
import Token
import File
import Resource
import Tree

import Control.Concurrent
import qualified Control.Exception as E
import System.Exit
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import Data.Maybe

-- import Network.HTTP.Conduit (newManager)
-- import Network.HTTP.Client (defaultManagerSettings)

mainLoop :: Int -> OAuth2WebServerFlow -> Maybe Token -> IO ()
mainLoop lastChange webFlow accessToken = do
  threadDelay 30000000
  putStrLn "Looping back"

  about <- getInformation accessToken webFlow
  case about of
       Nothing -> error "Could not get information!"
       (Just info) -> do

       	     	      let changeId = read (largestChangeId info) :: Int
		      if changeId > lastChange
		      then do

		      change <- getChange accessToken webFlow changeId
		      putStrLn $ show change
		      mainLoop changeId webFlow accessToken

		      else mainLoop lastChange webFlow accessToken

handler :: Int -> ThreadId -> OAuth2WebServerFlow -> IO ()
handler sig tid webFlow = do
  E.throwTo tid ExitSuccess
  endFlow webFlow
  putStrLn "Goodbye"

checkInfo :: Maybe About -> About
checkInfo Nothing = error "Could not get information!"
checkInfo (Just info) = info

main :: IO ()
main = do
--   manager <- newManager defaultManagerSettings
  webFlow <- createFlow "configuration" "authorization.txt" -- manager
  accessToken <- getTokens webFlow

  fileList <- getFileList accessToken webFlow
  createDirectory $ fromJust fileList

  about <- getInformation accessToken webFlow
  putStrLn $ show about

  tid <- myThreadId
  installHandler sigINT (Catch $ handler 0 tid webFlow) Nothing
  mainLoop (read $ largestChangeId $ checkInfo about) webFlow accessToken
