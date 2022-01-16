import Text.Parsec hiding (Line)
import Text.Parsec.String

import qualified Data.Map as M

import Debug.Trace

pBoard :: Parser [[Int]]
pBoard = sepBy pLine endOfLine <* eof

pLine :: Parser [Int]
pLine = many (read . (:[]) <$> digit)

toMap :: [[Int]] -> M.Map (Int, Int) Int
toMap xs = M.fromList $ concat $ zipWith (\line y -> zipWith (\energy x -> ((x, y), energy)) line [0..]) xs [0..]

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

flashLevel :: Int
flashLevel = 9

type Count = Int

step :: (M.Map (Int, Int) Int, Count) -> (M.Map (Int, Int) Int, Count)
step (m, cInit) = 
  let 
      initial = (M.map (\x -> (x+1, False)) m, cInit)
      (finalMap, finalCount) = fix flashMap initial
  in (M.map fst finalMap, finalCount)
  where
    flash :: (Int, Int) -> M.Map (Int, Int) (Int, Bool) -> M.Map (Int, Int) (Int, Bool)
    flash pos m' = M.adjust (const (0, True)) pos $ foldr (\nPos -> M.adjust (\(x, hasFlashed) -> (if hasFlashed then x else x+1, hasFlashed)) nPos) m' (neighbours pos)

    flashMap :: (M.Map (Int, Int) (Int, Bool), Count) -> (M.Map (Int, Int) (Int, Bool), Count)
    flashMap (m', c) = 
      let flashKeys = M.keys $ M.filter (\(level, hasFlashed) -> (level > flashLevel && not hasFlashed)) m'
      in (foldr flash m' flashKeys, length flashKeys + c)

    fix :: Eq a => (a -> a) -> a -> a 
    fix f x = let fx = f x in if x == fx then x else fix f fx

showBoard :: M.Map (Int, Int) Int -> String
showBoard m = concat $ map (maybe "\n" show . flip M.lookup m) [(x, y) | y <- [0..9], x <- [0..10]]

allZero :: M.Map (Int, Int) Int -> Bool
allZero = all (== 0)

untilCount :: (a -> a) -> (a -> Bool) -> a -> (a, Int)
untilCount f check x = go 0 x
  where go n y = let fy = f y in if check fy then (fy, n) else go (n + 1) fy

main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pBoard inputFile
  case res of 
    Left e -> print e
    Right board -> do
      let b1 = toMap board
      let ((res, resCount), iterationLoop) = untilCount step (\x -> allZero (fst x)) (b1, 0)
      print resCount
      print iterationLoop
      putStrLn (showBoard res)
      

