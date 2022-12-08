module Task8 (sol1, sol2) where

import Data.Char (digitToInt)
import Data.List (foldl', nub, transpose)

type Occ = [(Char, Int)]
type Crawl = (Char, Occ)

sol2 = 2

-- TODO: Map approach probably? go from (0,0) -> (99,99) and scout.

solveSet :: [(Int, Int, Char)] -> (Char, [(Int, Int, Char)])
solveSet l = foldl' f ('/', []) l -- '/' is pred '0'
  where f :: (Char, [(Int, Int, Char)]) -> (Int, Int, Char) -> (Char, [(Int, Int, Char)])
        f (m, l) (r,c,v) = if v > m then (v, (r,c,v):l) else (m,l)


sol1 :: IO Int
sol1 = length . nub . concatMap (concatMap (snd . solveSet)) . allLists <$> input

allLists :: [[(Int, Int, Char)]] -> [[[(Int, Int, Char)]]]
allLists l = [ltr l, rtl l, btt l, ttb l]


input :: IO [[(Int, Int, Char)]]
input = do
  file <- readFile "resources/input8.txt"
  return (flattenIndex (index (lines file)))


index :: [String] -> [(Int, [(Int, Char)])]
index s = zip [0..] (map (zip [0..]) s)

flattenIndex :: [(Int, [(Int, Char)])] -> [[(Int, Int, Char)]]
flattenIndex [] = []
flattenIndex ((i, l):xs) = map (\(a,b) -> (i,a,b)) l : flattenIndex xs

ltr :: [[(Int, Int, Char)]] -> [[(Int, Int, Char)]]
ltr = id

rtl :: [[(Int, Int, Char)]] -> [[(Int, Int, Char)]]
rtl = map reverse

ttb :: [[(Int, Int, Char)]] -> [[(Int, Int, Char)]]
ttb = transpose

btt :: [[(Int, Int, Char)]] -> [[(Int, Int, Char)]]
btt = transpose . reverse