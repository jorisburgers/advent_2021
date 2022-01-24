import Text.Parsec hiding (Line, Empty)
import Text.Parsec.String

import qualified Data.Map as M

data Square = Dot | Empty

instance Show Square where
  show Dot = "#"
  show Empty = "."

newtype Board = Board { boardMap :: M.Map (Int, Int) Square }

instance Show Board where
  show (Board b) = 
    let w = maximum $ map fst (M.keys b)
        h = maximum $ map snd (M.keys b)
        drawLine :: Int -> String
        drawLine y = concatMap (\x -> show $ M.findWithDefault Empty (x, y) b ) [0..w]
    in unlines (map drawLine [0..h])

data Instruction = FoldX Int | FoldY Int
  deriving (Show)

pBoardWithInstructions :: Parser (Board, [Instruction])
pBoardWithInstructions = (,) <$> pBoard <* endOfLine <*> pInstructions

pBoard :: Parser Board
pBoard = makeBoard <$> pBoardLines

pBoardLines :: Parser [(Int, Int)]
pBoardLines = many1 (pBoardLine <* endOfLine)

pBoardLine :: Parser (Int, Int)
pBoardLine = (,) <$> (read <$> many1 digit) <* char ',' <*> (read <$> many1 digit)

pInstructions :: Parser [Instruction]
pInstructions = sepBy pInstruction endOfLine <* eof

pInstruction :: Parser Instruction
pInstruction = string "fold along " *> ((string "x=" *> (FoldX . read <$> many1 digit)) <|> (string "y=" *> (FoldY . read <$> many1 digit)))

makeBoard :: [(Int, Int)] -> Board
makeBoard coords = Board $ M.fromList (map (\c -> (c, Dot)) coords )

executeInstruction :: Instruction -> Board -> Board
executeInstruction inst = uncurry overlay . splitFlip inst

splitFlip :: Instruction -> Board -> (Board, Board)
splitFlip instr = (\(l, r) -> (Board $ limit (<) l, Board $ flip $ limit (>=) r)) . M.partitionWithKey splitCondition . boardMap
  where
    splitCondition :: (Int, Int) -> Square -> Bool
    splitCondition = case instr of
      FoldX l -> (\(x, _) _ -> x < l)
      FoldY l -> (\(_, y) _ -> y < l)

    limit op = case instr of 
      FoldX h -> M.filterWithKey (\(x, _) _ -> x `op` h)
      FoldY w -> M.filterWithKey (\(_, y) _ -> y `op` w) 

    flip :: M.Map (Int, Int) a -> M.Map (Int, Int) a
    flip = case instr of 
      FoldX h -> M.filterWithKey (\(x, _) _ -> x < h) . M.mapKeys (\(x, y) -> (h - (x - h), y))
      FoldY w -> M.filterWithKey (\(_, y) _ -> y < w) . M.mapKeys (\(x, y) -> (x, w - (y - w)))
  
overlay :: Board -> Board -> Board
overlay b1 b2 = Board $ M.unionWith join (boardMap b1) (boardMap b2)
  where
    join Empty Empty = Empty
    join _ _ = Dot

countDots :: Board -> Int
countDots = length . M.filter isDot . boardMap
  where
    isDot Dot = True
    isDot _ = False 

main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pBoardWithInstructions inputFile
  case res of 
    Left e -> print e
    Right (board, instructions) -> do
      let res = foldl (flip executeInstruction) board instructions
      print res
      