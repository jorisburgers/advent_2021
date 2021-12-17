import Text.Parsec hiding (Line)
import Text.Parsec.String

import Data.List

import qualified Data.Map as M

data Line = Line [String] [String]
  deriving Show

pSegment :: Parser String 
pSegment = many1 (oneOf "abcdefg")

pSegments :: Parser [String]
pSegments = sepEndBy pSegment (char ' ')

pLine :: Parser Line
pLine = Line <$> pSegments <* string "| " <*> pSegments

pLines :: Parser [Line]
pLines = sepBy pLine endOfLine <* eof

decode :: M.Map String Int -> String -> Int
decode table s = maybe (error ("Unkown string " <> s <> " " <> show table )) id (M.lookup (sort s) table)

truthTable :: [String] -> M.Map Int String  
truthTable xs = 
  let 
    xs' = xs <> xs <> xs
    go :: M.Map Int String  -> [String] -> M.Map Int String 
    go m [] = error (show m)
    go m (x:xs) 
      | M.size m == 10 = m
      | otherwise =
          go (maybe m (\v -> M.insert v x m) (decipher m x)) xs
  in go M.empty xs' 

decipher :: M.Map Int String -> String -> Maybe Int
decipher m x 
  | length x == 2 = pure 1
  | length x == 3 = pure 7
  | length x == 4 = pure 4
  | length x == 7 = pure 8
  | length x == 5 = do
      x4 <- M.lookup 4 m
      x1 <- M.lookup 1 m
      if x `union` x1 == x then
        pure 3
      else do
        x9 <- M.lookup 9 m
        if x `intersect` x9 == x then
          pure 5
        else
          pure 2
  | length x == 6 = do
      x4 <- M.lookup 4 m
      x1 <- M.lookup 1 m
      if x `union ` x1 `union ` x4 == x then
        pure 9
      else if x `union` x1 == x then
        pure 0
      else 
        pure 6
  | otherwise = error (show (m, x))

decodeLine :: Line -> Int
decodeLine (Line uniques outputs) = 
  let 
    table = M.fromList $ map (\(x, y) -> (sort y, x)) $ M.toList $ truthTable uniques
  in foldl' (\acc x -> acc * 10 + x) 0 $ map (decode table) outputs

main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pLines inputFile
  case res of 
    Left e -> print e
    Right lines -> do
      print (sum $ map decodeLine lines)
      
