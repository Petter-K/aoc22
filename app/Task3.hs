module Task3 (sol1, sol2) where

import Data.List (nub, intersect)
import qualified Data.Map as Map

sol1 :: IO Int
sol1 = sum . map (\k -> Map.findWithDefault 0 k pri) . concatMap (nub . findCommons . parseSacks) <$> input
  where findCommons :: (String, String) -> String
        findCommons (l, r) = l `intersect` r

sol2 :: IO Int
sol2 = sum . map (\k -> Map.findWithDefault 0 k pri) . concatMap (nub . foldl1 intersect) . group 3 <$> input




pri :: Map.Map Char Int
pri = Map.fromList (zip ['a'..'z'] [1..] ++ zip ['A'..'Z'] [27..])

parseSacks :: String -> (String, String)
parseSacks s = splitAt (div (length s) 2) s

group :: Int -> [String] -> [[String]]
group _ [] = []
group n l = take n l : group n (drop n l)

input :: IO [String]
input = lines <$> readFile "resources/input3.txt"
