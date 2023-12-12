#!/usr/bin/env runhaskell

-- TODO:
-- 1. use a better data structure for the grid
-- 2. no need to do a whole search on numbers for each asterisk actually
module Main where

import           Data.Bifunctor (second)
import           Data.Char      (isDigit)
import           Data.List      (find, foldl', groupBy, intersect)

type IndexedLine = (Int, String)
type Dimension = (Int, Int)
type Pos = (Int, Int)

main :: IO ()
main = getContents >>= print . solve . lines

solve :: [String] -> Int
solve lines =
  let
    indexed = zip [0 ..] lines
    numbers = indexed >>= extract
    asteriskBoundaries = fmap (getBoundary (length lines, length $ head lines)) (indexed >>= findAsterisks)
   in
    foldl' (\acc boundary -> acc + findPartNumbers numbers boundary) 0 asteriskBoundaries

extract :: IndexedLine -> [(String, [Pos])]
extract (i, s) =
  let
    chunks = groupBy (\(x, _) (y, _) -> isDigit x && isDigit y) $ zip s [0 ..]
    numInfo = unzip <$> filter (all (isDigit . fst)) chunks
   in
    fmap (second (fmap (i,))) numInfo

findAsterisks :: IndexedLine -> [Pos]
findAsterisks (i, s) = fmap (\(_, j) -> (i, j)) . filter (\(x, _) -> x == '*') $ zip s [0 ..]

getBoundary :: Dimension -> Pos -> [Pos]
getBoundary dim@(m, n) pos@(row, col) = [(r, c) | r <- [row - 1 .. row + 1], r >= 0, r < m, c <- [col - 1 .. col + 1], c >= 0, c < n, (r, c) /= pos]

findPartNumbers :: [(String, [Pos])] -> [Pos] -> Int
findPartNumbers nums boundary = case filter (\(_, numPos) -> not . null $ intersect numPos boundary) nums of
  [(n1, _), (n2, _)] -> read n1 * read n2
  _                  -> 0
