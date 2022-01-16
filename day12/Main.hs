import Text.Parsec hiding (Line)
import Text.Parsec.String

import Data.Maybe (mapMaybe)

data Cave 
  = Start
  | End 
  | Large String
  | Small String
  deriving (Show, Eq, Ord)

isSmall :: Cave -> Bool
isSmall Small{} = True
isSmall _ = False

pCaves :: Parser [(Cave, Cave)]
pCaves = sepBy pCaveLine endOfLine <* eof

pCaveLine :: Parser (Cave, Cave)
pCaveLine = (,) <$> pCave <* char '-' <*> pCave

pCave :: Parser Cave
pCave 
  = Start <$ try (string "start")
  <|> End <$ try (string "end")
  <|> Large <$> many1 upper 
  <|> Small <$> many1 lower

neighbours :: [(Cave, Cave)] -> Cave -> [Cave]
neighbours haystack needle = mapMaybe neighbour haystack
  where
    neighbour (c1, c2)
      | c1 == needle = Just c2
      | c2 == needle = Just c1
      | otherwise = Nothing

canVisit :: [Cave] -> Cave -> Bool
canVisit visited current@(Small _) = not (current `elem` visited) || not (hasDouble (filter isSmall visited))
  where
    hasDouble [] = False
    hasDouble (x:xs) = x `elem` xs || hasDouble xs 
canVisit visited End = True
canVisit visited Start = not (Start `elem` visited)
canVisit visited (Large _) = True

makePaths :: [(Cave, Cave)] -> [[Cave]]
makePaths caves = go Start []
  where
    go :: Cave -> [Cave] -> [[Cave]]
    go End visited = [End : visited]
    go current visited 
      | not (canVisit visited current) = []
      | otherwise = concatMap (\next -> go next (current : visited)) (neighbours caves current)

main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pCaves inputFile
  case res of 
    Left e -> print e
    Right caves -> do
      print (length $ makePaths caves)
      
