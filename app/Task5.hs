module Task5 (sol1, sol2) where

import Data.List (transpose)
import qualified Data.Map as M

sol1 = Nothing
sol2 = Nothing


parseCrates s = 1
  where f :: String -> M.Map Int String -> M.Map Int String
        f s m = m 

input :: IO ([String], [String])
input = do 
  file <- lines <$> readFile "resources/input5.txt"
  let crates = takeWhile (/= "") file
  let cmds = drop (length crates + 2) file -- Dropping 1 2 3 row and an empty line. 
  return (crates, cmds)

file = readFile "resources/input5.txt"


test = do
  (h,_) <- input
  return h

-- transpose . take 9 . lines <$> file
-- group :: Int -> [String] -> [[String]]
group _ [] = []
group n l = take n l : group n (drop n l)