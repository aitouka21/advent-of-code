#!/usr/bin/env runhaskell

import           Control.Arrow (second)
import           Data.List     (intercalate)
import           Data.MemoTrie (memo2)

parse :: String -> (String, [Int])
parse = second (read . (\s -> "[" ++ s ++ "]")) . break (== ' ')

unfold :: (String, [Int]) -> (String, [Int])
unfold (s, cond) = (intercalate "?" (replicate 5 s), concat (replicate 5 cond))

solve :: String -> [Int] -> Int
solve = memo2 solve'
 where
  solve' [] [] = 1
  solve' [] _ = 0
  solve' ('#' : _) [] = 0
  solve' ('.' : xs) ys = solve xs ys
  solve' ('?' : xs) ys = solve xs ys + solve ('#' : xs) ys
  solve' xs (y : ys)
    | length xs == y, '.' `notElem` xs', null rest = solve [] ys
    | length xs > y, '.' `notElem` xs', head rest /= '#' = solve (tail rest) ys
    | otherwise = 0
   where
    (xs', rest) = splitAt y xs

main :: IO ()
main = do
  inputs <- map parse . lines <$> getContents
  let !p1 = sum [solve s cond | (s, cond) <- inputs]
  let !p2 = sum [solve s cond | input <- inputs, let (s, cond) = unfold input]
  print (p1, p2)
