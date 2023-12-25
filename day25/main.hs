module Main (main) where

import           Control.Arrow               ((***))
import qualified Data.Array.Unboxed          as A
import           Data.Complex                (realPart)
import           Data.Foldable               (foldl')
import           Data.List                   (sort, sortOn)
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as S
import           Data.Void                   (Void)
import qualified Numeric.LinearAlgebra       as LA
import qualified Numeric.LinearAlgebra.Devel as LA
import qualified Text.Megaparsec             as P
import qualified Text.Megaparsec.Char        as P
import qualified Text.Megaparsec.Char.Lexer  as P

type Vertex = Int
type Edge = (Vertex, Vertex)

-- TODO: study flow algorithm,
-- https://en.wikipedia.org/wiki/Karger%27s_algorithm
-- also understand this crazy black magic
-- https://en.wikipedia.org/wiki/Algebraic_connectivity#Partitioning_a_graph_using_the_Fiedler_vector
main :: IO ()
main = do
  Just input <- P.parseMaybe parser <$> getContents
  let m = Map.fromList . (`zip` [0::Int ..]) . S.toList . S.unions $ map (S.fromList . uncurry (:)) input
  let vs = map snd $ Map.toList m
  let es = map ((Map.!) m *** (Map.!) m) $ concatMap (\(a, bs) -> map (a, ) bs) input
  let (eigenvalues, eigenvectors) = LA.eig $ deg vs es - adj vs es
  let fiedlerVector = concat . LA.toLists . (eigenvectors LA.¿) . pure . fst . (!! 1) . sortOn snd . zip [0..] . map realPart $ LA.toList eigenvalues
  print . uncurry (*) . (length *** length) . span (< 0) . sort $ map realPart fiedlerVector

deg :: [Vertex] -> [Edge] -> LA.Matrix LA.R
deg vertices edges = LA.diag $ LA.vector $ map (fromIntegral . snd) $ Map.toAscList diag
  where
    dim = length vertices
    a = Map.fromList $ map (, 0 :: Int) vertices
    diag = foldl' (\m (va, vb) -> Map.adjust (+1) vb $ Map.adjust (+1) va m) a edges

adj :: [Vertex] -> [Edge] -> LA.Matrix LA.R
adj vertices edges = m
  where
    dim = length vertices
    zero = (dim LA.>< dim) (repeat 0 :: [LA.R])
    m = LA.mapMatrixWithIndex (\(i, j) _ -> if (i, j) `elem` edges || (j, i) `elem` edges then 1 else 0) zero

type Parser = P.Parsec Void String

parser :: Parser [(String, [String])]
parser = line `P.sepEndBy` P.eol

line :: Parser (String, [String])
line = (,) <$> label <* P.char ':' <*> P.many (P.char ' ' *> label)

label :: Parser String
label = P.many P.letterChar
