#!/usr/bin/env runhaskell

import Data.List
import Data.Map qualified as M
import Data.Maybe

index :: [String] -> [((Int, Int), Char)]
index = concat . zipWith (\x -> zipWith (\y c -> ((x, y), c)) [0 ..]) [0 ..]

main :: IO ()
main = do
  input <- index . lines <$> readFile "input.txt"
  let start = fromJust $ find ((== 'S') . snd) input
  print start

(<+>) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(<+>) (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)
