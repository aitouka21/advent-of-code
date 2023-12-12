#!/usr/bin/env runhaskell

{-# LANGUAGE ParallelListComp  #-}
{-# LANGUAGE TransformListComp #-}

module Main where

import           Control.Arrow ((&&&))
import           Data.List     (elemIndex, group, sort, sortBy)
import           Data.Maybe    (fromJust)
import           Data.Ord      (Down (..), comparing)
import           GHC.Exts      (sortWith)

dict1 = "23456789TJQKA"
dict2 = "J23456789TQKA"

-- TODO: This sucks. Rewrite this shit.

magic :: Char -> String
magic 'J' = tail dict2
magic c   = [c]

pos :: String -> Char -> Int
pos dict = fromJust . flip elemIndex dict

f :: String -> (String -> [String]) -> String -> [Int]
f dict magic s = maximum (map score hand) : map (pos dict) s
 where
  hand = sortBy (comparing Down) . map (length &&& head) . group . sort <$> magic s

  score hand = case map fst hand of
    [5]             -> 6
    [4, 1]          -> 5
    [3, 2]          -> 4
    [3, 1, 1]       -> 3
    [2, 2, 1]       -> 2
    [2, 1, 1, 1]    -> 1
    [1, 1, 1, 1, 1] -> 0

main :: IO ()
main = do
  input <- map ((\[hand, bid] -> (hand, read bid)) . words) . lines <$> getContents
  print $ sum [rank * bid | rank <- [1 ..] | (hand, bid) <- input, let s = f dict1 pure hand, then sortWith by s]
  print $ sum [rank * bid | rank <- [1 ..] | (hand, bid) <- input, let s = f dict2 (traverse magic) hand, then sortWith by s]
