import Text.Parsec hiding (Line)
import Text.Parsec.String

import Data.Maybe (mapMaybe)

import qualified Data.Map as M

pArea :: Parser [[Int]]
pArea = sepBy pLine endOfLine <* eof

pLine :: Parser [Int]
pLine = many pInt

pInt :: Parser Int
pInt = read . (:[]) <$> digit

constructAreaMap :: [[Int]] -> M.Map (Int, Int) Int
constructAreaMap xs = M.fromList $ concat $ zipWith (\line y -> zipWith (\height x -> ((x, y), height)) line [0..]) xs [0..]

neighbours :: M.Map (Int, Int) Int -> (Int, Int) -> [Int]
neighbours area (x, y) = mapMaybe (flip M.lookup area) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

lowestPoints :: M.Map (Int, Int) Int -> M.Map (Int, Int) Int
lowestPoints area = M.filterWithKey (\pos height -> all (> height) (neighbours area pos)) area

main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pArea inputFile
  case res of 
    Left e -> print e
    Right area -> do
      print (sum $ M.map (+1) $ lowestPoints $ constructAreaMap area)
      
