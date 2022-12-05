module Task5 (sol1, sol2) where

import qualified Data.Map as M

sol1 = Nothing
sol2 = Nothing


parseCrates s = 1
  where f :: String -> M.Map Int String -> M.Map Int String
        f s m = m 

-- input :: IO String
input = do 
  file <- lines <$> readFile "resources/input5.txt"
  let crates = takeWhile (/= " 1   2   3   4   5   6   7   8   9 ") file
  let cmds = drop (length crates + 2) file -- Dropping 1 2 3 row and an empty line. 
  return (crates, cmds)

file = readFile "resources/input5.txt"


-- group :: Int -> [String] -> [[String]]
group _ [] = []
group n l = take n l : group n (drop n l)