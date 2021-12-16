import Text.Parsec
import Text.Parsec.String

import qualified Data.Map as M

pFishes :: Parser [Int]
pFishes = sepBy pFish (char ',') <* eof

pFish :: Parser Int
pFish = read <$> many1 digit

newFish :: Int
newFish = 8

resetFish :: Int
resetFish = 6

construct :: [Int] -> M.Map Int Int
construct = foldr (\x -> M.insertWith (+) x 1) M.empty

iteration :: M.Map Int Int -> M.Map Int Int 
iteration m = 
  let newFishes = maybe 0 id (M.lookup 0 m) 
  in M.insert newFish newFishes (M.mapKeysWith (+) bumpFish m)
  where 
    bumpFish :: Int -> Int
    bumpFish 0 = resetFish
    bumpFish n = n - 1

iterations :: M.Map Int Int -> Int -> M.Map Int Int
iterations m 0 = m
iterations m n = iterations (iteration m) (n - 1)

main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pFishes inputFile
  case res of 
    Left e -> print e
    Right fishes -> do
      let initial = construct fishes
      print (M.foldr (+) 0 $ iterations initial 256)