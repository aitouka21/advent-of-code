#!/usr/bin/env runhaskell

import Data.Char (isDigit)

main :: IO ()
main = do
  [l1, l2] <- map (drop 10) . lines <$> getContents
  let p1 = product $ zipWith solve (read <$> words l1) (read <$> words l2)
  let p2 = solve (read (filter isDigit l1)) (read (filter isDigit l2))
  print (p1, p2)

solve b c = r1 - r2 + 1
 where
  delta = sqrt (b ^ 2 - 4 * c)
  floor' x
    | x < 1 = 0
    | otherwise = ceiling $ x - 1
  ceiling' = floor . (+ 1)
  r1 = floor' $ (b + delta) / 2
  r2 = ceiling' $ (b - delta) / 2
