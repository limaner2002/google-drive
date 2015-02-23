{-# LANGUAGE OverloadedStrings #-}
module File
    ( File,
      FileList,
      printFiles
    )
    where

import Data.Aeson
import Control.Applicative ((<$>),(<*>))
import Data.Maybe
import Control.Monad (mzero, void)
import qualified Data.ByteString.Lazy as BL
import Text.PrettyPrint.Boxes
import Data.List

data File = File
          { name :: String,
            kind :: String,
            mimeType :: String
          }

instance Show File
    where
      show file = "name: " ++ name file ++ " mimeType: " ++ mimeType file

instance FromJSON File
    where
      parseJSON (Object o) = File <$> o .: "title" <*>
                             o .: "kind" <*>
                             o .: "mimeType"
      parseJSON _ = mzero

data FileList = FileList
    { files :: [File]
    }

instance Show FileList
    where
      show (FileList []) = ""
      show (FileList (f:fs)) = show f++"\n"++show (FileList fs)

instance FromJSON FileList
    where
      parseJSON (Object o) = do
        files <- parseJSON =<< (o .: "items")
        return $ FileList files

printTable :: [[String]] -> IO ()
printTable rows = printBox $ hsep 2 left (map (vcat left . map text) (transpose rows))

printFiles_ :: FileList -> [[String]]
printFiles_ (FileList []) = []
printFiles_ (FileList (f:fs)) = [name f, mimeType f]:printFiles_ (FileList fs)

printFiles :: Maybe FileList -> IO ()
printFiles Nothing = putStrLn "Nothing"
printFiles (Just fl) = printTable $ printFiles_ fl
