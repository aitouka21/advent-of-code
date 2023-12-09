#!/usr/bin/env runhaskell

readData :: FilePath -> IO [[Int]]
readData filename = do
  contents <- readFile filename
  pure $ map (map read . words) $ lines contents

main :: IO ()
main = do
  inputs <- readData "input.txt"
  let p1 = sum $ map (solve . reverse) inputs
  let p2 = sum $ map solve inputs
  print (p1, p2)

solve :: [Int] -> Int
solve xs@(x : xs') = if all (== 0) xs then 0 else x + solve (zipWith (-) xs xs')
