{-# LANGUAGE TransformListComp #-}

module Main where

import           Data.Array.Unboxed (Array, array, bounds, inRange, range, (!))
import           Data.Char          (digitToInt)
import qualified Data.Set           as S
import           GHC.Exts           (sortWith)

type Pos = (Int, Int)
data Dir = U | D | L | R deriving (Show, Eq, Ord)
data Node = N Pos Dir Int deriving (Show, Eq, Ord)

arr :: [String] -> Array Pos Int
arr xss = array ((0, 0), (m, n)) [((i, j), digitToInt $ xss !! i !! j) | i <- [0 .. m], j <- [0 .. n]]
 where
  (m, n) = (length xss - 1, length (head xss) - 1)

solve :: Array Pos Int -> [Int] -> Pos -> Pos -> (Int, [Node])
solve m steps start end = f S.empty [(0, [N start U 0])]
 where
  f seen ((d, path) : qs)
    | pos == end = (d, reverse path)
    | otherwise = f seen' $ merge qs' qs
   where
    N pos dir step = head path

    next =
      [ n
      | step' <- steps , dir' <- turn dir
      , let pos' = go pos (dir', step'), inRange (bounds m) pos'
      , let n = N pos' dir' step', n `S.notMember` seen
      ]

    qs' =
      [ (d + d', n : path)
      | n@(N pos' _ _) <- next
      , let d' = sum (map (m !) $ range (min pos pos', max pos pos')) - m ! pos
      , then sortWith by d'
      ]

    seen' = S.union seen $ S.fromList next

merge :: [(Int, [Node])] -> [(Int, [Node])] -> [(Int, [Node])]
merge [] ys = ys
merge xs [] = xs
merge xs@(x : xs') ys@(y : ys')
  | fst x < fst y = x : merge xs' ys
  | otherwise = y : merge xs ys'

turn :: Dir -> [Dir]
turn U = [L, R]
turn D = [L, R]
turn L = [U, D]
turn R = [U, D]

go :: Pos -> (Dir, Int) -> Pos
go (x, y) (U, n) = (x - n, y)
go (x, y) (D, n) = (x + n, y)
go (x, y) (L, n) = (x, y - n)
go (x, y) (R, n) = (x, y + n)

-- TODO: terrible performance LOL, enhance it later
main :: IO ()
main = do
  input <- arr . lines <$> getContents
  let (start, end) = bounds input
  let (!p1, _) = solve input [1..3] start end
  let (!p2, _) = solve input [4..10] start end
  print (p1, p2)
