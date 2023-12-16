#!/usr/bin/env runhaskell

module Main where

import           Data.Array (Array, array, bounds, inRange, (!))
import qualified Data.Set   as S

type Pos = (Int, Int)
data Dir = U | D | L | R deriving (Eq, Ord, Show)

arr :: [String] -> Array (Int, Int) Char
arr xss = array ((0, 0), (m, n)) [((i, j), xss !! i !! j) | i <- [0 .. m], j <- [0 .. n]]
 where (m, n) = (length xss - 1, length (head xss) - 1)

nextDir :: Char -> Dir -> [Dir]
nextDir '.' dir = [dir]
nextDir '/' L   = [D]
nextDir '/' U   = [R]
nextDir '/' R   = [U]
nextDir '/' D   = [L]
nextDir '\\' L  = [U]
nextDir '\\' D  = [R]
nextDir '\\' R  = [D]
nextDir '\\' U  = [L]
nextDir '|' U   = [U]
nextDir '|' D   = [D]
nextDir '|' _   = [U, D]
nextDir '-' L   = [L]
nextDir '-' R   = [R]
nextDir '-' _   = [L, R]

go :: Pos -> Dir -> Pos
go (x, y) U = (x - 1, y)
go (x, y) D = (x + 1, y)
go (x, y) L = (x, y - 1)
go (x, y) R = (x, y + 1)

run :: Array (Int, Int) Char -> (Pos, Dir) -> S.Set (Pos, Dir)
run m (start, dir) = f S.empty [(start, dir') | dir' <- nextDir (m ! start) dir]
 where
  f s [] = s
  f s (x : xs)
    | x `S.member` s = f s xs
    | otherwise = f (S.insert x s) (xs ++ move x)

  move (pos, dir)
    | not (inRange (bounds m) pos') = []
    | otherwise = map (pos',) $ nextDir (m ! pos') dir
    where pos' = pos `go` dir

solve :: Array (Int, Int) Char -> [(Pos, Dir)] -> Int
solve m = maximum . map (length . S.map fst . run m)

starts :: (Int, Int) -> [(Pos, Dir)]
starts (m, n) =  [((x, y), d) | x <- [0 .. m], (y, d) <- [(0, R), (n, L)]]
              ++ [((x, y), d) | y <- [0 .. n], (x, d) <- [(0, D), (m, U)]]

main :: IO ()
main = do
  m <- arr . lines <$> getContents
  let !p1 = solve m [((0, 0), R)]
  let !p2 = solve m $ starts (snd $ bounds m)
  print (p1, p2)
