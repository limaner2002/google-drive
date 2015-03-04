module Tree
    where

import qualified Data.Map as M
import qualified Data.Tree as T
import Data.List (foldl')
import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)

import File
import Data.List

data Rec a = Rec
    { recId :: String,
      parentId :: Maybe String,
      value :: a
    } deriving Show

toTree :: [Rec a] -> [T.Tree a]
toTree rs = ts
       where
	gs = foldl' f M.empty rs where
	     	      f m record = M.insertWith (const (record:)) (parentId record) [record] m

	t = T.unfoldTree mkNode (undefined, Nothing)
	mkNode (a, i) = (a, map (value &&& Just . recId)
	       	      	    . fromMaybe []
       	             	    . M.lookup i $ gs)
	ts = T.subForest t

breadthFirst :: (Show a, Eq a) => T.Tree a -> [String]
breadthFirst t = bf [t]
    where
      bf [] = []
      bf xs = concat (map values xs) ++ (bf $ validChildren xs)

      validChildren xs = concat $ map T.subForest xs

      values node = map (\chld -> (show $ T.rootLabel node) ++ "->" ++ (show $ T.rootLabel chld)) (T.subForest node)

createDirectory :: FileList -> IO ()
createDirectory (FileList files _) = do
  let l = map (\x -> Rec (fid x) (pid x) (value x)) $ filter (\x -> length (parents x) /= 0) files
          where
            fid = File.id . fileResource
            pid = Just . File.id . parentResource . head . parents
            value x = name x
  let tree = toTree ([Rec "0ABG_oYXlfrNQUk9PVA" Nothing "Root"]++l)
  mapM_ (putStrLn . T.drawTree) tree
  -- putStrLn "digraph G{"
  -- mapM_ putStrLn $ breadthFirst $ head tree
  -- putStrLn "}"
