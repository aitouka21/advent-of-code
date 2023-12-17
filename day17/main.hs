module Main where

import Data.Array
import Data.Char
import Data.List
import Data.Set qualified as S

type Pos = (Int, Int)
data Dir = U | D | L | R deriving (Show, Eq, Ord)

arr :: [String] -> Array Pos Int
arr xss = array ((0, 0), (m, n)) [((i, j), digitToInt $ xss !! i !! j) | i <- [0 .. m], j <- [0 .. n]]
 where
  (m, n) = (length xss - 1, length (head xss) - 1)

main :: IO ()
main = do
  input <- arr . lines <$> getContents
  let (start, end) = bounds input
  let p1 = run input start end
  let p2 = "I fucked up"
  print (p1, p2)

run :: Array Pos Int -> Pos -> Pos -> Int
run m start end = f S.empty [(0, [(start, (U, 0))])]
 where
  f seen ((score, path) : qs)
    | node == end = score
    | otherwise = f seen' qs'
   where
    (node, dir) = head path
    xs' = [x' | x'@(pos', (_, acc')) <- next (node, dir), x' `S.notMember` seen, pos' `notElem` map fst (take 3 path), inRange (bounds m) pos', acc' < 4]
    qs' = merge qs $ sort [(score + m ! pos', x' : path) | x'@(pos', _) <- xs']
    seen' = S.union seen $ S.fromList xs'

merge :: [(Int, a)] -> [(Int, a)] -> [(Int, a)]
merge [] ys = ys
merge xs [] = xs
merge xs@(x : xs') ys@(y : ys')
  | fst x < fst y = x : merge xs' ys
  | otherwise = y : merge xs ys'

next :: (Pos, (Dir, Int)) -> [(Pos, (Dir, Int))]
next ((i, j), (dir, count)) =
  [ ((i + 1, j), nextDir D)
  , ((i - 1, j), nextDir U)
  , ((i, j + 1), nextDir R)
  , ((i, j - 1), nextDir L)
  ]
 where
  nextDir dir' = (dir', if dir == dir' then count + 1 else 1)
