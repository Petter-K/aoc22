module Task7 (sol1, sol2) where

data Loc = File Int | Dir [Loc] deriving (Eq, Show)

sol1 = Nothing
sol2 = Nothing

size :: Loc -> Int
size (File s) = s
size (Dir d) = sum (map size d)