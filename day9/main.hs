#!/usr/bin/env runhaskell

main :: IO ()
main = do
  inputs <- map (map read . words) . lines <$> readFile "input.txt"
  let p1 = sum $ map (solve . reverse) inputs
  let p2 = sum $ map solve inputs
  print (p1, p2)

solve :: [Int] -> Int
solve xs@(x : xs') = if all (== 0) xs then 0 else x + solve (zipWith (-) xs xs')
