import Text.Parsec
import Text.Parsec.String

pFishes :: Parser [Int]
pFishes = sepBy pFish (char ',') <* eof

pFish :: Parser Int
pFish = read <$> many1 digit

newFish :: Int
newFish = 8

resetFish :: Int
resetFish = 6

iteration :: [Int] -> [Int]
iteration = go []
  where 
    go new [] = new
    go new (x:xs) 
      | x == 0 = resetFish : go (newFish:new) xs
      | otherwise = (x - 1) : go new xs

iterations :: [Int] -> Int -> [Int]
iterations list 0 = list
iterations list n = iterations (iteration list) (n - 1)

main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pFishes inputFile
  case res of 
    Left e -> print e
    Right fishes -> do
      print (length $ iterations fishes 80)