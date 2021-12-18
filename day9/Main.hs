import Text.Parsec hiding (Line)
import Text.Parsec.String

import Data.Maybe (mapMaybe)
import Data.List (sort)

import qualified Data.Map as M

pArea :: Parser [[Int]]
pArea = sepBy pLine endOfLine <* eof

pLine :: Parser [Int]
pLine = many pInt

pInt :: Parser Int
pInt = read . (:[]) <$> digit

constructAreaMap :: [[Int]] -> M.Map (Int, Int) Int
constructAreaMap xs = M.fromList $ concat $ zipWith (\line y -> zipWith (\height x -> ((x, y), height)) line [0..]) xs [0..]

neighboursCoords :: (Int, Int) -> [(Int, Int)]
neighboursCoords (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

neighbours :: M.Map (Int, Int) Int -> (Int, Int) -> [Int]
neighbours area pos = mapMaybe (flip M.lookup area) (neighboursCoords pos)

lowestPoints :: M.Map (Int, Int) Int -> M.Map (Int, Int) Int
lowestPoints area = M.filterWithKey (\pos height -> all (> height) (neighbours area pos)) area

basin :: M.Map (Int, Int) Int -> (Int, Int) -> M.Map (Int, Int) Int
basin area pos = go M.empty pos
  where
    go curBasin curPos 
      | not (curPos `M.member` area) = M.empty
      | M.lookup curPos area == Just 9 = M.empty
      | curPos `M.member` curBasin = M.empty
      | otherwise = foldr (\nPos b -> M.union b (go b nPos)) (M.insert curPos (area M.! curPos) curBasin) (neighboursCoords curPos)

main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pArea inputFile
  case res of 
    Left e -> print e
    Right area -> do
      let areaMap = constructAreaMap area
      let basins = map (basin areaMap) $ M.keys (lowestPoints areaMap)
      print (product $ take 3 $ reverse $ sort $ map (\b -> M.size b) basins)
      
