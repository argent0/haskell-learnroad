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
import Control.Monad

class KeyValueContainer c where
   insert :: Ord k => c k v -> (k, v) -> c k v
   insertList :: Ord k => c k v -> [(k,v)] -> c k v
   insertList bt as = foldr (flip insert) bt as
   search :: Ord k => c k v -> k -> Maybe v

data BSTree k v = NullTree | Tip (k, v) | BSTree {
   node :: (k, v),
	left :: BSTree k v,
	right :: BSTree k v
} deriving Show

instance KeyValueContainer BSTree where
   insert NullTree (nk, nv) = Tip (nk, nv)

   insert ot@(Tip (k, v)) (nk, nv) = case (compare nk k) of
      LT -> BSTree {
         node = (k, v),
         left = Tip (nk, nv),
         right = NullTree }
      GT -> BSTree {
         node = (k, v),
         left = NullTree,
         right = Tip (nk, nv) }
      EQ -> Tip (nk, nv)

   insert ot@(BSTree {node = (k,v), left = lt, right = rt}) (nk,nv) = case (compare nk k) of
      LT -> BSTree {
         node = (k,v),
         left = (insert	lt (nk, nv)),
         right=rt}
      GT -> BSTree {
         node = (k,v),
         left = lt,
         right = (insert rt (nk, nv))}
      EQ -> BSTree {
         node = (nk, nv),
         left = lt,
         right = rt}

   search NullTree sk = Nothing
   search (Tip (k, v)) sk 
      | (k == sk) = Just v
      | otherwise = Nothing
   search (BSTree {node=(k,v), left=lt, right=rt}) sk
      | (sk == k) = Just v
      | (sk < k) = search lt sk
      | (sk > k) = search rt sk

splitString :: Char -> String -> ([Char],[Char])
splitString c str = case (elemIndex c str) of
	Nothing -> (str,"")
	Just idx -> let (h,t) = (splitAt idx str) in (h, tail t)

splitRecord :: Char -> String -> Either [Char] ([Char], [Char])
splitRecord c str = case (elemIndex c str) of
	Just idx -> let (h,t) = (splitAt idx str) in Right (h, tail t)
	Nothing -> Left $ "Malformed Record: " ++ str ++ "\n"

		
read_frequencies :: Handle -> IO (Either [Char] (BSTree String Int))
read_frequencies inputFileHandle = do
	eof <- hIsEOF inputFileHandle
	if eof
		then return $ Right NullTree
		else do
			inputStr <- hGetLine inputFileHandle
			case (splitRecord '\t' inputStr) of
				Left e -> return $ Left e
				Right (k,v) -> case (reads v :: [(Int,String)]) of
					[(iv,"")] -> (read_frequencies inputFileHandle >>= \et ->
						case et of
							Left e -> return $ Left e
							Right t -> return $ Right $ insert t (k, iv))
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

main :: IO ()
main = do
   putStrLn "Welcome to (string,int) key-value search engine!"
   args <- getArgs
   putStrLn $ "Reading file: " ++ show (args !! 0)
   inTree <- read_tree_from_file (args !! 0)
   case inTree of
      Left e -> putStrLn e
      Right tree -> make_queries_from_stdin tree 

-- vim: expandtab
