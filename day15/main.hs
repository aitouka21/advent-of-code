#!/usr/bin/env runhaskell

import           Data.Char     (ord)
import           Data.Foldable (foldl')
import qualified Data.Map      as M

splitOn :: Char -> String -> [String]
splitOn c s = case span (/= c) s of
  (s, []) -> [s]
  (s, s') -> s : splitOn c (tail s')

data Command = Set String Int | Remove String deriving (Show)

label :: Command -> String
label (Set s _)  = s
label (Remove s) = s

command :: String -> Command
command s = case splitOn '=' s of
  [s, n] -> Set s (read n)
  [s]    -> Remove (init s)

hash :: String -> Int
hash = foldl' f 0
  where f acc c = (acc + ord c) * 17 `mod` 256

op :: Command -> [(String, Int)] -> [(String, Int)]
op (Remove s) xs      = filter ((/= s) . fst) xs
op (Set s n) []       = [(s, n)]
op c@(Set s n) (x:xs) | fst x == s = (s, n) : xs
                      | otherwise  = x : op c xs

exec :: [Command] -> M.Map Int [(String, Int)]
exec = foldl' adjust (M.fromList $ map (, []) [0..255])
  where adjust m c = M.adjust (op c) (hash (label c)) m

sum' :: M.Map Int [(String, Int)] -> Int
sum' = M.foldlWithKey' g 0
  where g acc hash xs = sum (zipWith (\pos l -> (hash + 1) * pos * l) [1..] (map snd xs)) + acc

main :: IO ()
main = do
  [inputs] <-  map (splitOn ',') . lines <$> getContents
  let p1 = sum $ map hash inputs
  let p2 = sum' $ exec $ map command inputs
  print (p1, p2)
