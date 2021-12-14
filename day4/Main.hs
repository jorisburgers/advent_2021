

import Text.Parsec
import Text.Parsec.String

data Board = Board [Cell]
  deriving Show

-- (Right, Down)
type Position = (Int, Int)

data Cell = Cell 
  { cellPosition :: Position
  , cellMarked :: Bool
  , cellValue :: Int
  }
  deriving Show

-- | The maximum size of the board, so this represents a 5x5 board
maxSize :: Int
maxSize = 4

pEntries :: Parser [Int]
pEntries = sepBy (read <$> many1 digit) (char ',')

pEntriesBoards :: Parser ([Int], [Board])
pEntriesBoards = (,) <$> pEntries <* endOfLine <* endOfLine <*> pBoards

pBoards :: Parser [Board]
pBoards = many pBoard

pCell :: (Int, Int) -> Parser Cell
pCell pos@(x, y) = Cell pos False <$> pDigit <* end
  where
    pDigit = read <$> (
      ((:[]) <$> (char ' ' *> digit)) 
      <|> 
      ((\d1 d2 -> [d1, d2]) <$> digit <*> digit) 
      )
    end | y == maxSize && x == maxSize = (() <$ endOfLine <* endOfLine) <|> eof
        | x == maxSize = () <$ endOfLine
        | otherwise = () <$ char ' '
 
pBoard :: Parser Board
pBoard = do
  let positions = [(x, y) | y <- [0..maxSize], x <- [0..maxSize]]
  Board <$> mapM pCell positions

mark :: Int -> Board -> Board
mark val (Board cells) = Board (map mark' cells)
  where 
    mark' :: Cell -> Cell
    mark' c
      | val == cellValue c = c{cellMarked = True}
      | otherwise = c

filterPosition :: ((Int, Int) -> Int) -> Board -> [[Cell]]
filterPosition f (Board cells) = map (\rowColIndex -> filter (\c -> f (cellPosition c) == rowColIndex) cells) [0..maxSize]

rows :: Board -> [[Cell]]
rows = filterPosition snd

colls :: Board -> [[Cell]]
colls = filterPosition fst

hasBingo :: Board -> Bool
hasBingo board = any (all cellMarked) (rows board) || any (all cellMarked) (colls board)

applyUntilBingo :: [Board] -> [Int] -> (Int, Board)
applyUntilBingo [] _ = error "No boards left"
applyUntilBingo _ [] = error "No entries left"
applyUntilBingo boards (v:vs) =
  let markedBoards = map (mark v) boards
      bingoBoards = filter hasBingo markedBoards
  in case bingoBoards of
    -- We have a bingo board, return it
    (x:_) -> (v, x )
    _ -> applyUntilBingo markedBoards vs

unmarkedSum :: Board -> Int
unmarkedSum (Board cells) = sum $ map cellValue $ filter (not . cellMarked) cells

main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pEntriesBoards inputFile
  case res of 
    Left e -> print e
    Right (entries, boards) -> do
      let (lastValue, winningBoard) = applyUntilBingo boards entries
      print (unmarkedSum winningBoard * lastValue)