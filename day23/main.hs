module Main (main) where

import qualified Data.Array.Unboxed as A
import qualified Data.Map.Strict    as M
import qualified Data.Set           as S

type Pos = (Int, Int)
type Map = A.UArray Pos Char
data Me = Me { prev :: Pos, cur :: Pos, cost :: Int } deriving (Show)

arr :: [[Char]] -> Map
arr xss = A.array ((0, 0), (n, m)) assocs
  where
    n = length xss - 1
    m = length (head xss) - 1
    assocs = [((i, j), x) | (i, xs) <- zip [0 ..] xss, (j, x) <- zip [0 ..] xs]

go :: Pos -> (Int, Int) -> Pos
go (x, y) (dx, dy) = (x + dx, y + dy)

dirs :: [(Int, Int)]
dirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]
run :: ((Int, Int) -> Char -> Bool) -> Map -> Pos -> [Pos] -> [Me]
run valid map start ends = f $ Me start start 0
  where
    f me@(Me prev cur cost)
      | cur `elem` ends, cur /= start = [me]
      | otherwise = [x' | x <- moves, x' <- f $ Me cur x (cost + 1)]
      where moves = [ move
                    | dir <- dirs
                    , let move = cur `go` dir , A.inRange (A.bounds map) move , move /= prev , valid dir (map A.! move)
                    ]

valid :: (Int, Int) -> Char -> Bool
valid _       '.' = True
valid (-1, 0) '^' = True
valid (1, 0)  'v' = True
valid (0, -1) '<' = True
valid (0, 1)  '>' = True
valid _       _   = False

-- Part 2
valid' :: (Int, Int) -> Char -> Bool
valid' _       '#' = False
valid' _       _   = True

junctions :: Map -> [Pos]
junctions m = [ pos | pos <- A.indices m, m A.! pos /= '#'
              , length [ dir
                       | dir <- dirs
                       , let move = pos `go` dir , A.inRange (A.bounds m) move , m A.! move /= '#'
                       ] > 2
              ]

run' :: Map -> Pos -> Pos -> [Int]
run' m start end = f S.empty start
  where
    nodes = start : junctions m
    parse (Me _ pos cost) = (pos, cost)
    next pos = map parse $ run valid' m pos (end:nodes)
    nmap = M.fromList $ zip nodes (map next nodes)

    f seen pos
      | pos == end = [0]
      | otherwise = [ cost + cost' | (pos', cost) <- nmap M.! pos, pos' `S.notMember` seen , cost' <- f (S.insert pos' seen) pos' ]

main :: IO ()
main = do
  input <- arr . lines <$> getContents
  let (_, (n, m)) = A.bounds input
  let (start, end) = ((0, 1), (n, m - 1))
  let !p1 = maximum $ map cost $ run valid input start [end]
  let !p2 = maximum $ run' input start end
  print (p1, p2)
