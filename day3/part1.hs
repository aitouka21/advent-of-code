#!/usr/bin/env runhaskell

module Main where

-- Approach:
-- Cosider every symbol except . are colorized
-- Shift the sample up and down by 1 and then overlay them
--
--  =====================   <- shifted up
--   =====================  <- original
--    ===================== <- shifted down
--
-- Then the sum of the part numbers will be every colorized number and number
-- that is enclosed by colorized symbol
-- THIS SOLUTION SUCKS AND DON'T WORK FOR PART 2 LMAO
-- Should use another approach
data Point = Point {isColorized :: Bool, char :: Char}

instance Show Point where
  show (Point True c) = "\ESC[31m" ++ show c ++ "\ESC[0m"
  show (Point False c) = show c

toInt :: [Point] -> Int
toInt = read . fmap char

isDigit :: Point -> Bool
isDigit (Point _ c) = c `elem` ['0' .. '9']

from :: Char -> Point
from c = Point (c `notElem` '.' : ['0' .. '9']) c

empty :: Point
empty = from '.'

overlay :: [[Point]] -> [[Point]] -> [[Point]]
overlay = zipWith (zipWith (\(Point b c) p' -> Point (b || (char p' `notElem` '.' : ['0' .. '9'])) c))

-- this is a mess LMAO
parseIt :: [Point] -> [[Point]]
parseIt xs =
  case dropWhile (not . (\x -> isDigit x || isColorized x)) xs of
    [] -> []
    x : xs
      | isDigit x && isColorized x -> (x : xs') : remaining
      | isColorized x -> xs' : remaining
      | isDigit x -> case (xs', rest) of
          (n, _) | any isColorized n -> (x : xs') : remaining
          (_, endWith : _) | isColorized endWith -> (x : xs') : remaining
          _ -> remaining
     where
      (xs', rest) = span isDigit xs
      remaining = parseIt rest

main :: IO ()
main = do
  f <- readFile "input.txt"
  let colorized = fmap (fmap from) . lines $ f

  let layers =
        [ tail colorized ++ [repeat empty]
        , repeat empty : init colorized
        ]

  let overlaid = foldl overlay colorized layers
  let values = overlaid >>= fmap toInt . filter (not . null) . parseIt

  print (sum values)
