#!/usr/bin/env runhaskell

import Control.Applicative (liftA2)
import Data.Char (isDigit)

main :: IO ()
main = readFile "input.txt" >>= print . liftA2 (,) (solver p1) (solver p2) . lines

solver :: (String -> String) -> [String] -> Int
solver parser = sum . fmap (read . (<*>) [head, last] . pure . parser)

p1 :: String -> String
p1 = filter isDigit

p2 :: String -> String
p2 [] = []
p2 s@(x : xs)
  | x `elem` ['1' .. '9'] = x : p2 xs
  | otherwise = case s of
      'o' : 'n' : 'e' : _ -> '1' : p2 xs
      't' : 'w' : 'o' : _ -> '2' : p2 xs
      't' : 'h' : 'r' : 'e' : 'e' : _ -> '3' : p2 xs
      'f' : 'o' : 'u' : 'r' : _ -> '4' : p2 xs
      'f' : 'i' : 'v' : 'e' : _ -> '5' : p2 xs
      's' : 'i' : 'x' : _ -> '6' : p2 xs
      's' : 'e' : 'v' : 'e' : 'n' : _ -> '7' : p2 xs
      'e' : 'i' : 'g' : 'h' : 't' : _ -> '8' : p2 xs
      'n' : 'i' : 'n' : 'e' : _ -> '9' : p2 xs
      _ -> p2 xs
