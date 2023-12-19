module Main where

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Function
import Data.List
import Data.Map qualified as M
import Data.Maybe
import Data.Void
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

main :: IO ()
main = do
  [xs, _, ys] <- groupBy ((==) `on` null) . lines <$> getContents
  let ins = M.fromList $ map (fromJust . parseMaybe insP) xs
  let inputs = map (fromJust . parseMaybe input) ys

  let p1 = sum [x + m + a + s | i@(Input x m a s) <- inputs, exec ins (Jump "in") i == "A"]
  let dim = (1, 4000)
  let p2 = sim ins (Jump "in") (Just (Input dim dim dim dim))
  print p2

data V = X | M | A | S deriving (Show)

type Range = (Int, Int)

data Input a = Input a a a a deriving (Show)

data Ins
  = Jump String
  | GreaterThan V Int Ins Ins
  | LessThan V Int Ins Ins
  deriving (Show)

get :: V -> Input a -> a
get X (Input x _ _ _) = x
get M (Input _ m _ _) = m
get A (Input _ _ a _) = a
get S (Input _ _ _ s) = s

set :: V -> Input a -> a -> Input a
set X (Input _ m a s) x = Input x m a s
set M (Input x _ a s) m = Input x m a s
set A (Input x m _ s) a = Input x m a s
set S (Input x m a _) s = Input x m a s

exec :: M.Map String Ins -> Ins -> Input Int -> String
exec m = eval
 where
  eval (Jump "A") _ = "A"
  eval (Jump "R") _ = "R"
  eval (Jump label) i = eval (m M.! label) i
  eval (GreaterThan v n l r) i
    | get v i > n = eval l i
    | otherwise = eval r i
  eval (LessThan v n l r) i
    | get v i < n = eval l i
    | otherwise = eval r i

sim :: M.Map String Ins -> Ins -> Maybe (Input Range) -> Int
sim _ (Jump "R") _ = 0
sim _ _ Nothing = 0
sim _ (Jump "A") (Just (Input x m a s)) = dist x * dist m * dist a * dist s
sim m (Jump label) i = sim m (m M.! label) i
sim m ins (Just i) = sim m l lr + sim m r rr
 where
  (l, r) = branch ins
  (lr, rr) = splitRange ins i

splitRange :: Ins -> Input Range -> (Maybe (Input Range), Maybe (Input Range))
splitRange (GreaterThan v n _ _) i = bimap (fmap (set v i)) (fmap (set v i)) $ forGT n (get v i)
splitRange (LessThan v n _ _) i = bimap (fmap (set v i)) (fmap (set v i)) $ forLT n (get v i)

branch :: Ins -> (Ins, Ins)
branch (GreaterThan _ _ l r) = (l, r)
branch (LessThan _ _ l r) = (l, r)

forGT :: Int -> Range -> (Maybe Range, Maybe Range)
forGT n (low, hi)
  | n < low = (Just (low, hi), Nothing)
  | n >= hi = (Nothing, Just (low, hi))
  | otherwise = (Just (n + 1, hi), Just (low, n))

forLT :: Int -> Range -> (Maybe Range, Maybe Range)
forLT n (low, hi)
  | n <= low = (Nothing, Just (low, hi))
  | n > hi = (Just (low, hi), Nothing)
  | otherwise = (Just (low, n - 1), Just (n, hi))

dist :: Range -> Int
dist (low, hi) = hi - low + 1

type Parser = Parsec Void String

insP :: Parser (String, Ins)
insP = (,) <$> jlbl <*> brace stmt

jlbl :: Parser String
jlbl = takeWhile1P (Just "label") isAlpha

stmt :: Parser Ins
stmt = try cmpStmt <|> jumpStmt
 where
  cmpStmt = do
    v <- choice [X <$ char 'x', M <$ char 'm', A <$ char 'a', S <$ char 's']
    cond <- char '>' <|> char '<'
    n <- decimal
    void $ char ':'
    stmt1 <- stmt
    void $ char ','
    stmt2 <- stmt
    return $ case cond of
      '>' -> GreaterThan v n stmt1 stmt2
      '<' -> LessThan v n stmt1 stmt2

  jumpStmt = Jump <$> takeWhile1P (Just "label") isAlpha

input :: Parser (Input Int)
input = brace $ Input <$> extract "x" <*> extract "m" <*> extract "a" <*> extract "s"
 where
  extract key = do
    void $ string key
    void $ char '='
    val <- decimal
    void $ optional $ char ','
    return val

brace :: Parser a -> Parser a
brace = between (char '{') (char '}')
