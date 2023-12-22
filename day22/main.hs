{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import           Data.List
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

data Coord = Coord { x :: Int, y :: Int, z :: Int } deriving (Show, Eq, Ord)
data Brick = Brick { start :: Coord, end :: Coord } deriving (Show, Eq, Ord)

setZ :: Int -> Brick -> Brick
setZ z brick = brick { start = (start brick) { z = z }, end = (end brick) { z = z + (brick.end.z - brick.start.z) } }

-- FIXME: SKILL ISSUE AGAIN. my haskell solution fucked up but I don't know why
-- fix it when I have time, just do it in js first
main :: IO ()
main = do
  Just bricks <- fmap (sortOn (z . start)) . parseMaybe input <$> getContents
  let bricks' = foldl' fall [] bricks
  print $ length $ filter (canDisintegrate bricks') bricks'

overlap :: Brick -> Brick -> Bool
overlap a b = (a.start.x <= b.end.x && b.start.x <= a.end.x)
           && (a.start.y <= b.end.y && b.start.y <= a.end.y)

-- TODO: Why I set the z index to 1?????
fall :: [Brick] -> Brick -> [Brick]
fall [] brick = [setZ 1 brick]
fall (b:bs) brick
  | overlap brick b = setZ (b.end.z + 1) brick : b : bs
  | otherwise       = b : fall bs brick

supportBy :: [Brick] -> Brick -> [Brick]
supportBy bricks brick = [b | b <- bricks, b.start.z == brick.end.z + 1, overlap brick b]

supportOf :: [Brick] -> Brick -> [Brick]
supportOf bricks brick = [b | b <- bricks, b.end.z == brick.start.z - 1, overlap brick b]

canDisintegrate :: [Brick] -> Brick -> Bool
canDisintegrate bricks brick = null [b | b <- supportBy bricks brick, supportOf bricks b == [brick]]

input :: Parsec Void String [Brick]
input = line `endBy1` newline <* eof
  where
    line  = Brick <$> coord <* char '~' <*> coord
    coord = Coord <$> decimal <* char ',' <*> decimal <* char ',' <*> decimal
