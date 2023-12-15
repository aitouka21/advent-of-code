#!/usr/bin/env runhaskell

import           Data.Char       (ord)
import           Data.Foldable   (Foldable (foldl'))
import           Data.List.Split (splitOn)
import qualified Data.Map        as M

data Command = Set String Int | Remove String deriving (Show)

label :: Command -> String
label (Set s _)  = s
label (Remove s) = s

command :: String -> Command
command s = case splitOn "=" s of
  [s, n] -> Set s (read n)
  [s]    -> Remove (init s)

m ::  M.Map Int [(String, Int)]
m = M.fromList $ map (, []) [0..255]

hash :: String -> Int
hash = foldl' f 0
  where f acc c = (acc + ord c) * 17 `mod` 256

op :: Command -> [(String, Int)] -> [(String, Int)]
op (Remove s) xs      = filter ((/= s) . fst) xs
op (Set s n) []       = [(s, n)]
op c@(Set s n) (x:xs) | fst x == s = (s, n) : xs
                     | otherwise  = x : op c xs

adjust :: Command -> M.Map Int [(String, Int)] -> M.Map Int [(String, Int)]
adjust command = M.adjust (op command) (hash (label command))

sum' :: M.Map Int [(String, Int)] -> Int
sum' = M.foldlWithKey' g 0
  where g acc hash xs = sum (zipWith (\pos length -> (hash + 1) * pos * length) [1..] (map snd xs)) + acc

main :: IO ()
main = do
  [inputs] <-  map (splitOn ",") . lines <$> getContents
  let p1 = sum $ map hash inputs
  let p2 = sum' $ foldl' (flip adjust) m $ map command inputs
  print (p1, p2)
