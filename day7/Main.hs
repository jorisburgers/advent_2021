import Text.Parsec
import Text.Parsec.String

import qualified Data.Map as M

pCrabs :: Parser [Int]
pCrabs = sepBy pCrab (char ',') <* eof

pCrab :: Parser Int
pCrab = read <$> many1 digit

moveToCost :: [Int] -> Int -> Int
moveToCost xs target = sum (map (\x -> abs (x - target)) xs)

allCosts :: [Int] -> M.Map Int Int
allCosts xs = M.fromList [(x, moveToCost xs x) | x <- [minimum xs .. maximum xs]]

main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pCrabs inputFile
  case res of 
    Left e -> print e
    Right crabs -> do
      print (minimum (allCosts crabs))