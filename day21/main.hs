module Main where

import Data.Array.Unboxed
import Data.Set qualified as S

type Map = UArray (Int, Int) Char

arr :: [[Char]] -> Map
arr xss = array ((0, 0), (n, m)) [((i, j), xss !! i !! j) | i <- [0 .. n], j <- [0 .. m]]
 where
  n = length xss - 1
  m = length (head xss) - 1

next :: (Int, Int) -> [(Int, Int)]
next (i, j) = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

-- >>> ref (65, 131)
-- (65,0)
ref :: (Int, Int) -> (Int, Int)
ref (i, j) = (i `mod` 131, j `mod` 131)

g :: Map -> S.Set (Int, Int) -> S.Set (Int, Int)
g m s = s'
 where
  f pos = S.fromList [pos' | pos' <- next pos, m ! ref pos' /= '#']
  s' = S.unions $ S.map f s

f :: Int -> Int
f n = 14773 * n * n + 14863 * n + 3730

main :: IO ()
main = do
  input <- arr . lines <$> getContents
  let counts = map length $ iterate (g input) (S.singleton (65, 65))
  let p1 = counts !! 64
  print (counts !! 65, counts !! 196, counts !! 327) -- quadratic fit using these 3 numbers lol
  print (p1, f (26501365 `div` 131))
