module Task2 (sol1, sol2) where

data Shape = Rock | Paper | Scissor deriving (Eq, Show)

sol1 :: IO Int
sol1 = sum . map (score . parseLine1) <$> input

sol2 :: IO Int
sol2 = sum . map (score . parseLine2) <$> input

points :: Shape -> Int
points Rock = 1
points Paper = 2
points Scissor = 3

score :: (Shape, Shape) -> Int
score (Rock, Paper) = 6 + points Paper
score (Paper, Scissor) = 6 + points Scissor
score (Scissor, Rock) = 6 + points Rock
score (x,y) = (if x == y then 3 else 0) + points y

parseLine1 :: String -> (Shape, Shape)
parseLine1 [x,_,y] = (parseCol1 x, parseCol2 y)
  where parseCol2 :: Char -> Shape
        parseCol2 'X' = Rock
        parseCol2 'Y' = Paper
        parseCol2 'Z' = Scissor

parseLine2 :: String -> (Shape, Shape)
parseLine2 [x,_,'X'] = lose (parseCol1 x)
parseLine2 [x,_,'Y'] = draw (parseCol1 x)
parseLine2 [x,_,'Z'] = win (parseCol1 x)

lose :: Shape -> (Shape, Shape)
lose Rock = (Rock, Scissor)
lose Scissor = (Scissor, Paper)
lose Paper = (Paper, Rock)

win :: Shape -> (Shape, Shape)
win Rock = (Rock, Paper)
win Scissor = (Scissor, Rock)
win Paper = (Paper, Scissor)

draw :: Shape -> (Shape, Shape)
draw x = (x,x)

parseCol1 :: Char -> Shape
parseCol1 'A' = Rock
parseCol1 'B' = Paper
parseCol1 'C' = Scissor

input :: IO [String]
input = lines <$> readFile "resources/input2.txt"