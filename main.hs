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
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe

mainLoop :: Int -> Flow ()
mainLoop lastChange = do
  liftIO $ threadDelay 30000000
  checkToken'
  webFlow <- get

  change <- getChange lastChange
  let changeId = read (largestChangeId change) :: Int
  if changeId >= lastChange
  then do
    changeList <- listChanges lastChange (nextPageToken change)
    -- liftIO $ putStrLn $ show changeList
    processChange change
    let largestChange = read (largestChangeId changeList) :: Int
    mainLoop (largestChange+1)

  else do
    mainLoop lastChange

handler :: Int -> ThreadId -> OAuth2WebServerFlow -> IO ()
handler sig tid webFlow = do
  E.throwTo tid ExitSuccess
  endFlow webFlow
  putStrLn "Goodbye"

start :: Flow Int
start = do
     getTokens
     about <- getInformation
     liftIO $ putStrLn $ show about

     fileList <- listFiles (Just [])
     liftIO $ createDirectory fileList

     let lastChange = (read $ largestChange about) + 1

     mainLoop lastChange
     return 0

main :: IO ()
main = do
  webFlow <- createFlow "configuration" "authorization.txt"

  tid <- myThreadId
  installHandler sigINT (Catch $ handler 0 tid webFlow) Nothing

  let t = evalStateT . runExceptT $ start
  retVal <- t webFlow
  putStrLn $ show retVal
  putStrLn "Done!"
