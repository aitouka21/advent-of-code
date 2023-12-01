import           Control.Monad (ap, liftM2)
import           Data.Char     (isDigit)

p1 :: IO ()
p1 = solver (filter isDigit)

p2 :: IO ()
p2 = solver f
  where
    f [] = []
    f s@(x : xs)
      | x `elem` ['1' .. '9'] = x : f xs
      | otherwise = case take 5 s of
         'o' : 'n' : 'e' : _             -> '1' : f xs
         't' : 'w' : 'o' : _             -> '2' : f xs
         't' : 'h' : 'r' : 'e' : 'e' : _ -> '3' : f xs
         'f' : 'o' : 'u' : 'r' : _       -> '4' : f xs
         'f' : 'i' : 'v' : 'e' : _       -> '5' : f xs
         's' : 'i' : 'x' : _             -> '6' : f xs
         's' : 'e' : 'v' : 'e' : 'n' : _ -> '7' : f xs
         'e' : 'i' : 'g' : 'h' : 't' : _ -> '8' : f xs
         'n' : 'i' : 'n' : 'e' : _       -> '9' : f xs
         _                               -> f xs

solver :: (String -> String) -> IO ()
solver parser = readFile "input.txt" >>= print . sum . map (read . ap [head, last] . pure . parser) . lines

