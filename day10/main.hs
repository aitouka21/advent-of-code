#!/usr/bin/env runhaskell

import           Data.List (find)
import           Data.Map  (Map, fromList, (!))

main :: IO ()
main = do
  input <- indexed . lines <$> getContents
  let Just (start, _) = find ((== 'S') . snd) input
  let loop = solve (fromList input) start
  -- number of steps is half the length of the loop
  let steps = length loop `div` 2
  -- shoelace formula with pick's theorem
  let tiles = sum (zipWith det loop (tail $ cycle loop)) `div` 2 - steps + 1
  print (steps, tiles)

indexed :: [String] -> [((Int, Int), Char)]
indexed rows = [((x, y), c) | (x, row) <- zip [0 ..] rows, (y, c) <- zip [0 ..] row]

solve :: Map (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
solve m start = f (start `go` down) [start]
 where
  f curr xs
    | curr == start = xs
    | otherwise = f (curr `go` next) (curr : xs)
   where
    choice d1 d2 = if curr `go` d1 == head xs then d2 else d1
    next = case m ! curr of
      '|' -> choice up down
      '-' -> choice left right
      '7' -> choice left down
      'L' -> choice up right
      'J' -> choice up left
      'F' -> choice down right

up, down, left, right :: (Int, Int)
up = (-1, 0)
down = (1, 0)
left = (0, -1)
right = (0, 1)

go :: (Int, Int) -> (Int, Int) -> (Int, Int)
go (x, y) (x', y') = (x + x', y + y')

det :: (Int, Int) -> (Int, Int) -> Int
det (x1, y1) (x2, y2) = (x1 * y2) - (x2 * y1)
