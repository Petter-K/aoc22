module Task1 (sol1, sol2) where

import Data.List (sortOn)

sol1 :: IO Int
sol1 = maximum <$> input

sol2 :: IO Int
sol2 = sum . take 3 . sortOn negate <$> input

input :: IO [Int]
input = do
  file <- readFile "resources/input1.txt"
  return $ map (sum . map read) (splitInput $ lines file)

splitInput :: [String] -> [[String]]
splitInput f = go f [] []
  where go :: [String] -> [String] -> [[String]] -> [[String]]
        go [] l a = l : a
        go (x:xs) l a = if x == "" then go xs [] (l:a) else go xs (x:l) a