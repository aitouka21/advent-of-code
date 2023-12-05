#!/usr/bin/env runhaskell

module Main where

import Data.Function
import Data.List
import Data.Range

blocks :: String -> [[String]]
blocks input = filter (not . null . head) $ groupBy ((==) `on` (not . null)) $ lines input

readData :: FilePath -> IO ([Int], [[(Int, Int, Int)]])
readData filename = do
  [x] : xs <- blocks <$> readFile filename
  let seeds = map read $ words $ drop 7 x
  let maps = map (map ((\[x, y, z] -> (read x, read y, read z)) . words) . tail) xs
  pure (seeds, maps)

modify :: [Range Int] -> [(Int, Int, Int)] -> [Range Int]
modify r [] = r
modify r ((des, src, l) : ms) = mapped ++ modify remaining ms
 where
  remaining = difference r [src +=* (src + l)]
  mapped = fmap (+ (des - src)) <$> difference r remaining

ranges :: [Int] -> [Range Int]
ranges [] = []
ranges (x : y : xs) = (x +=* (x + y)) : ranges xs

main :: IO ()
main = do
  (seeds, maps) <- readData "input.txt"
  let p1 = head $ fromRanges $ foldl' modify (map SingletonRange seeds) maps
  let p2 = head $ fromRanges $ foldl' modify (ranges seeds) maps
  print (p1, p2)
