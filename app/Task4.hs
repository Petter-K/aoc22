module Task4 (sol1, sol2) where

import Data.List.Split (splitOn)
import Data.List (intersect, null)

sol1 :: IO Int
sol1 = sum . map (fromEnum . f) <$> input
  where f :: ((Int, Int), (Int, Int)) -> Bool
        f ((x,y), (a,b)) = x >= a && y <= b || a >= x && b <= y

sol2 :: IO Int
sol2 = sum . map (fromEnum . f) <$> input
  where f :: ((Int, Int), (Int, Int)) -> Bool
        f ((x,y), (a,b)) = not . null $ [x..y] `intersect` [a..b]

parse :: String -> ((Int, Int), (Int, Int))
parse s = let [h,t] = splitOn "," s
              [x,y] = splitOn "-" h
              [a,b] = splitOn "-" t
          in ((read x, read y), (read a, read b))

input :: IO [((Int, Int), (Int, Int))]
input = do
  file <- readFile "resources/input4.txt"
  return $ map parse $ lines file