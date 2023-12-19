module Main where

import Data.Bifunctor
import Data.Char
import Data.Map qualified as M
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

main :: IO ()
main = do
  Just (ins, inputs) <- parseMaybe parseContent <$> getContents
  let insMap = M.fromList ins
  let p1 = sum [x + m + a + s | i@(Input x m a s) <- inputs, exec insMap (Jump "in") i == "A"]
  let p2 = let dim = (1, 4000) in sim insMap (Jump "in") (Just (Input dim dim dim dim))
  print (p1, p2)

data Part = X | M | A | S deriving (Show)

type Range = (Int, Int)

data Input a = Input a a a a deriving (Show)

data Ins
  = Jump String
  | GreaterThan Part Int Ins Ins
  | LessThan Part Int Ins Ins
  deriving (Show)

get :: Part -> Input a -> a
get X (Input x _ _ _) = x
get M (Input _ m _ _) = m
get A (Input _ _ a _) = a
get S (Input _ _ _ s) = s

set :: Part -> Input a -> a -> Input a
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
    n <- decimal <* char ':'
    (stmt1, stmt2) <- (,) <$> stmt <* char ',' <*> stmt
    return $ case cond of
      '>' -> GreaterThan v n stmt1 stmt2
      '<' -> LessThan v n stmt1 stmt2

  jumpStmt = Jump <$> jlbl

inputP :: Parser (Input Int)
inputP = brace $ Input <$> extract "x" <*> extract "m" <*> extract "a" <*> extract "s"
 where
  extract key = string key *> char '=' *> decimal <* optional (char ',')

brace :: Parser a -> Parser a
brace = between (char '{') (char '}')

parseContent :: Parser ([(String, Ins)], [Input Int])
parseContent = (,) <$> insP `endBy1` newline <* newline <*> inputP `endBy1` newline <* eof
