#!/usr/bin/env runhaskell

import           Data.Function (on)
import           Data.List     (groupBy, inits, tails, transpose)

parse :: String -> [[String]]
parse = filter (not . null . head) . groupBy ((==) `on` null) . lines

solve :: ([String] -> [String] -> Bool) -> [String] -> Int
solve f m = case (reflectionLine m, reflectionLine (transpose m)) of
  ([], [i]) -> i
  ([i], []) -> 100 * i
 where
  reflectionLine m =
    [i | (i, l, r) <- zip3 [0 ..] (inits m) (tails m), l /= [], r /= [], f (reverse l) r]

mirror :: [String] -> [String] -> Bool
mirror a b = and $ zipWith (==) a b

oneDiff :: [String] -> [String] -> Bool
oneDiff a b = [True] == filter id (zipWith (/=) (concat a) (concat b))

main :: IO ()
main = do
  input <- fmap parse getContents
  let p1 = sum $ map (solve mirror) input
  let p2 = sum $ map (solve oneDiff) input
  print (p1, p2)
