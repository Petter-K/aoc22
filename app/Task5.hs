module Task5 (sol1, sol2) where

import Data.Char (isLetter, isDigit)
import Data.List (transpose, foldl')
import Data.List.Split (splitWhen)

import qualified Data.Map as M

type Command = [Int]
type Crates = M.Map Int String


sol1 :: IO String
sol1 = do
  crates <- initialCrates
  cmds <- commands
  let res = foldl' handleCommandSingle crates cmds
  return (map (head . snd) (M.assocs res))

sol2 :: IO String
sol2 = do
  crates <- initialCrates
  cmds <- commands
  let res = foldl' handleCommandMulti crates cmds
  return (map (head . snd) (M.assocs res))

handleCommandSingle :: Crates -> Command -> Crates
handleCommandSingle m (0:_) = m
handleCommandSingle m [c,f,t] = let (x:xs) = m M.! f
                          in handleCommandSingle (M.adjust (x :) t (M.insert f xs m)) [c-1, f, t]

handleCommandMulti :: Crates -> Command -> Crates
handleCommandMulti m (0:_) = m
handleCommandMulti m [c,f,t] = let l = m M.! f
                               in M.adjust (take c l ++) t (M.insert f (drop c l) m)


commands :: IO [Command]
commands = map (map read . filter (not . null) . splitWhen (not . isDigit)) . drop 10 . lines <$> file

initialCrates :: IO Crates
initialCrates = M.fromList . zip [1..] . filter (not . null) . map (filter isLetter) . transpose . takeWhile (/= "") . lines <$> file

file :: IO String
file = readFile "resources/input5.txt"