#!/usr/bin/env runhaskell

module Main where

import           Control.Applicative (liftA2)
import           Data.Char           (isAlpha, isSpace)
import qualified Data.Map            as M

readData :: IO ([(String, (String, String))], String)
readData = do
  x : _ : xs <- lines <$> getContents
  let nodes = words . filter (liftA2 (||) isAlpha isSpace) <$> xs
  return (map (\[a, b, c] -> (a, (b, c))) nodes, cycle x)

main :: IO ()
main = do
  (nodes, ins) <- readData
  let f = solve (M.fromList nodes) 0 ins
  let p1 = f ("AAA", (==) "ZZZ")
  let p2 = foldl1 lcm [f (start, (== 'Z') . last) | start <- filter ((== 'A') . last) $ fst <$> nodes]
  print (p1, p2)

-- ??? by luck?
solve :: M.Map String (String, String) -> Int -> String -> (String, String -> Bool) -> Int
solve m step (i : ins) (cur, end)
  | end cur = step
  | otherwise = solve m (step + 1) ins (next, end)
 where
  next =
    let v = (M.! cur) m
     in case i of
          'L' -> fst v
          'R' -> snd v
