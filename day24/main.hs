module Main (main) where

import           Data.Either                (isRight)
import qualified Data.Matrix                as Mat
import           Data.Void                  (Void)
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as P

type V2 = (Rational, Rational)
type V3 = (Rational, Rational, Rational)
type Matrix = Mat.Matrix Rational

p1 :: (V3, V3) -> (V3, V3) -> (Matrix, Matrix, Rational -> V2)
p1 a b = (mA, mB, solve)
  where
    ((p1x, p1y, _), (v1x, v1y, _)) = a
    ((p2x, p2y, _), (v2x, v2y, _)) = b

    mA = Mat.fromLists [[v1x, -v2x],[v1y, -v2y]]
    mB = Mat.fromLists [[p2x - p1x],[p2y - p1y]]

    solve t = (p1x + v1x * t, p1y + v1y * t)

inRange :: Ord a => (a, a) -> (a, a) -> Bool
inRange (from, to) (x, y) = x >= from && x <= to && y >= from && y <= to

p2 :: [(V3, V3)] -> Rational
p2 vs = sum $ take 3 $ Mat.toList m
  where
    [((p1x, p1y, p1z), (v1x, v1y, v1z)), ((p2x, p2y, p2z), (v2x, v2y, v2z)), ((p3x, p3y, p3z), (v3x, v3y, v3z))] = take 3 vs

    mA = Mat.fromLists [ [ v2y - v1y, v1x - v2x, 0, p1y - p2y, p2x - p1x, 0]
                       , [ v3y - v1y, v1x - v3x, 0, p1y - p3y, p3x - p1x, 0]
                       , [ 0, v2z - v1z, v1y - v2y, 0, p1z - p2z, p2y - p1y]
                       , [ 0, v3z - v1z, v1y - v3y, 0, p1z - p3z, p3y - p1y]
                       , [ v2z - v1z, 0, v1x - v2x, p1z - p2z, 0, p2x - p1x]
                       , [ v3z - v1z, 0, v1x - v3x, p1z - p3z, 0, p3x - p1x]
                       ]

    mB = Mat.fromLists [ [(p1y * v1x - p2y * v2x) - (p1x * v1y - p2x * v2y)]
                       , [(p1y * v1x - p3y * v3x) - (p1x * v1y - p3x * v3y)]
                       , [(p1z * v1y - p2z * v2y) - (p1y * v1z - p2y * v2z)]
                       , [(p1z * v1y - p3z * v3y) - (p1y * v1z - p3y * v3z)]
                       , [(p1z * v1x - p2z * v2x) - (p1x * v1z - p2x * v2z)]
                       , [(p1z * v1x - p3z * v3x) - (p1x * v1z - p3x * v3z)]
                       ]

    Right m = Mat.multStd <$> Mat.inverse mA <*> pure mB

main :: IO ()
main = do
  Just input <- P.parseMaybe parser <$> getContents

  let range = (200000000000000, 400000000000000)

  let pairs = [ intersection
              | a <- input, b <- input, a < b
              , let (mA, mB, solve) = p1 a b
              , let solution = Mat.multStd <$> Mat.inverse mA <*> pure mB
              , isRight solution, let Right [t1, t2] = fmap Mat.toList solution
              , t1 >= 0, t2 >= 0
              , let intersection = solve t1, inRange range intersection
              ]

  print (length pairs, p2 input)

type Parser = P.Parsec Void String

vec :: Parser V3
vec = (,,) <$> si <* sep <*> si <* sep <*> si
  where si  = P.signed (pure ()) P.decimal
        sep = P.char ',' <* P.space1

line :: Parser (V3, V3)
line = (,) <$> vec <* P.space1 <* P.char '@' <* P.space1 <*> vec

parser :: Parser [(V3, V3)]
parser = line `P.sepEndBy` P.eol
