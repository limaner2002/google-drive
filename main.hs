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
mainLoop lastChange webFlow tok = do
  threadDelay 30000000

  accessToken <- checkToken webFlow tok

  change <- getChange accessToken webFlow lastChange
  case change of
       Nothing -> error "Could not get change information!"
       (Just chg) -> do
       	     	      let changeId = read (largestChangeId chg) :: Int
		      if changeId >= lastChange
		      then do

		      -- change <- getChange accessToken webFlow (lastChange)
		      changeList <- listChanges accessToken webFlow lastChange (change >>= nextPageToken) >>= checkChangeList
		      putStrLn $ show changeList
		      let largestChange = read (largestChangeId changeList) :: Int
		      mainLoop (largestChange+1) webFlow accessToken

		      else mainLoop lastChange webFlow accessToken

checkChangeList :: Maybe Change -> IO Change
checkChangeList Nothing = error "Something went wrong getting the change list."
checkChangeList (Just change) = return change

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
  webFlow <- createFlow "configuration" "authorization.txt" -- manager
  accessToken <- getTokens webFlow

  fileList <- getFileList accessToken webFlow
  createDirectory $ fromJust fileList

  about <- getInformation accessToken webFlow
  putStrLn $ show about

  tid <- myThreadId
  installHandler sigINT (Catch $ handler 0 tid webFlow) Nothing
  mainLoop ((read (largestChange $ checkInfo about))+1) webFlow accessToken
