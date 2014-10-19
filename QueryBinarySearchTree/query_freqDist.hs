{- 

Copyright (C) 2014  Aner Luero

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

-}

{-
 - Given a file(as first CL paramter)  with lines:
 -
 - String \t Int
 -
 - It interprets it as key value pair. It loads all the pairs in a binary
 - search tree. It let's you search for keys and retrives the value.
 -
 - It's slow when compiled with: ghc file.hs. It's even slower if interpreted
 - with runghc file.hs
 -}

import System.Environment (getArgs)
import System.IO
import Data.List (elemIndex)

class KeyValueContainer c where
   insert :: Ord k => c k v -> k -> v -> c k v
   insertList :: Ord k => c k v -> [(k,v)] -> c k v
   insertList bt as = foldr (\(k,v) t -> insert t k v) bt as
   search :: Ord k => c k v -> k -> Maybe v

data BSTree k v = EmptyTree | Leaf k v | Branch k v (BSTree k v) (BSTree k v) deriving Show

instance KeyValueContainer BSTree where
   insert EmptyTree k v = Leaf k v 
   insert (Leaf k v) nk nv = case (compare nk k) of
      LT -> Branch k v (Leaf nk nv) EmptyTree
      GT -> Branch k v EmptyTree (Leaf nk nv)
      EQ -> Leaf k nv

   insert (Branch k v l r) nk nv = case (compare nk k) of
      LT -> Branch k v (insert l nk nv) r
      GT -> Branch k v l (insert r nk nv)
      EQ -> Branch k nv l r

   search EmptyTree _ = Nothing
   search (Leaf k v) sk
      | k == sk = Just v
      | otherwise = Nothing

   search (Branch k v l r) sk =
      case (compare sk k) of
         LT -> search l sk
         GT -> search r sk
         EQ -> Just v

splitRecord :: Char -> String -> Either [Char] ([Char], [Char])
splitRecord c str = case (elemIndex c str) of
	Just idx -> let (h,t) = (splitAt idx str) in Right (h, tail t)
	Nothing -> Left $ "Malformed Record: " ++ str ++ "\n"

		
read_frequencies :: Handle -> IO (Either [Char] (BSTree String Int))
read_frequencies inputFileHandle = do
	eof <- hIsEOF inputFileHandle
	if eof
		then return $ Right EmptyTree
		else do
			inputStr <- hGetLine inputFileHandle
			case (splitRecord '\t' inputStr) of
				Left e -> return $ Left e
				Right (k,v) -> case (reads v :: [(Int,String)]) of
					[(iv,"")] -> (read_frequencies inputFileHandle >>= \et ->
						case et of
							Left e -> return $ Left e
							Right t -> return $ Right $ insert t k iv)
					otherwise -> return $ Left "Could not convert to int"

read_tree_from_file :: FilePath -> IO (Either [Char] (BSTree String Int))
read_tree_from_file fp = do
   inputFile <- openFile fp ReadMode
   tree <- read_frequencies inputFile
   hClose inputFile
   return tree

make_queries_from_stdin :: (BSTree String Int) -> IO ()
make_queries_from_stdin tree = do
   putStr "query> "
   hFlush stdout
   eof <- hIsEOF stdin
   if (eof)
      then putStrLn "\nGood Bye!" >> return ()
      else hGetLine stdin >>= \ query ->
         putStrLn ("Searching for :<" ++ query ++ ">") >>= \_ -> 
         return (search tree query) >>= \ freq ->
         case freq of
            Just f -> putStrLn ("#" ++ query ++ "\t" ++(show f)) >>= \_ ->
               make_queries_from_stdin tree
            Nothing -> putStrLn("#" ++ query ++ "\t0") >>= \_ ->
               make_queries_from_stdin tree

gpl3_notice :: String
gpl3_notice = unlines ["QueryBinaryTree  Copyright (C) 2014 Aner Lucero",
   "This program comes with ABSOLUTELY NO WARRANTY",
   "This is free software, and you are welcome to redistribute it",
   "under certain conditions"]

main :: IO ()
main = do
   putStrLn gpl3_notice
   putStrLn "\nWelcome to (string,int) key-value search engine!"
   args <- getArgs
   putStrLn $ "Reading file: " ++ show (args !! 0)
   inTree <- read_tree_from_file (args !! 0)
   case inTree of
      Left e -> putStrLn e
      Right tree -> make_queries_from_stdin tree 

-- vim: expandtab
