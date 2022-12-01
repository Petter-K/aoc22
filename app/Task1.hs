module Task1 where

import Data.List.Split
import Data.List

solution1 :: IO Int
solution1 = maximum <$> input

solution2 :: IO Int
solution2 = sum . take 3 . reverse . sort <$> input

input :: IO [Int]
input = do
  file <- readFile "resources/input1.txt"
  return $ map (sum . map read) (splitOn [""] $ lines file)