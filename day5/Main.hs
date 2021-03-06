import Text.Parsec
import Text.Parsec.String

import qualified Data.Map as M

type Position = (Int, Int)

data Instruction = Instruction Position Position
  deriving Show

pInstructions :: Parser [Instruction]
pInstructions = sepBy pInstruction endOfLine <* eof

pInstruction :: Parser Instruction
pInstruction = Instruction <$> pPosition <* string " -> " <*> pPosition

pPosition :: Parser Position
pPosition = (,) <$> (read <$> many1 digit) <* char ',' <*> (read <$> many1 digit)

isLinear :: Instruction -> Bool
isLinear (Instruction p1 p2) = fst p1 == fst p2 || snd p1 == snd p2

isDiagonal :: Instruction -> Bool
isDiagonal (Instruction p1 p2) = abs (fst p1 - fst p2) == abs (snd p1 - snd p2)

draw :: [Instruction] -> M.Map Position Int
draw = foldr drawInstruction M.empty . concatMap expand
  where
    drawInstruction :: Position -> M.Map Position Int -> M.Map Position Int
    drawInstruction position = M.insertWith (+) position 1

    expand :: Instruction -> [Position]
    expand i@(Instruction p1 p2) 
      | isLinear i = [(x, y) | x <- listFrom (fst p1) (fst p2), y <- listFrom (snd p1) (snd p2)]
      | isDiagonal i = zipWith (,) (listFromOrd (fst p1) (fst p2)) (listFromOrd (snd p1) (snd p2))
      | otherwise = error "Should be linear or diagonal"

    listFrom :: Int -> Int -> [Int]
    listFrom a b = [min a b .. max a b]

    listFromOrd :: Int -> Int -> [Int]
    listFromOrd a b = enumFromThenTo a (if a < b then a + 1 else a - 1) b

main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pInstructions inputFile
  case res of 
    Left e -> print e
    Right instructions -> do
      let fInstructions = filter (\x -> isLinear x || isDiagonal x) instructions
      print (M.size $ M.filter (>= 2) $  draw fInstructions)