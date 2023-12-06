#!/usr/bin/env runhaskell

module Main where

import Control.Monad (join)
import Data.Function (on)
import Data.List (foldl', groupBy)

blocks :: String -> [[String]]
blocks input = filter (not . null . head) $ groupBy ((==) `on` (not . null)) $ lines input

readData :: FilePath -> IO ([Int], [[(Int, Int, Int)]])
readData filename = do
  [x] : xs <- blocks <$> readFile filename
  let seeds = map read $ words $ drop 7 x
  let maps = map (map ((\[x, y, z] -> (read x, read y, read z)) . words) . tail) xs
  pure (seeds, maps)

{- | consider the problem as translating the intersction of the given intervals and each map's domain [c_n, d_n]
with a fixed distance des - src, so the resulted segments of interval will be:

let modified = ∪ [c_n + dist_n, d_n + dist_n] in modified ∪ ([a, b] \ modified)
-}
modify :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int)]
modify [] r = [r]
modify ((des, src, l) : ms) (a, b)
  | d < a || c > b = modify ms (a, b)
  | c <= a && b <= d = [translate (a, b)]
  | a < c && d < b = translate (c, d) : (modify ms (a, c - 1) ++ modify ms (d + 1, b))
  | a < c && b <= d = translate (c, b) : modify ms (a, c - 1)
  | c <= a && d < b = translate (a, d) : modify ms (d + 1, b)
 where
  (c, d) = (src, src + l - 1)
  translate (x, y) = let dist = des - src in (x + dist, y + dist)

rangeSeeds :: [Int] -> [(Int, Int)]
rangeSeeds [] = []
rangeSeeds (x : y : xs) = (x, x + y - 1) : rangeSeeds xs

solve :: [(Int, Int)] -> [[(Int, Int, Int)]] -> Int
solve seeds maps = minimum $ fst <$> foldl' (flip (concatMap . modify)) seeds maps

main :: IO ()
main = do
  (seeds, maps) <- readData "input.txt"
  let p1 = solve (map (join (,)) seeds) maps
  let p2 = solve (rangeSeeds seeds) maps
  print (p1, p2)
