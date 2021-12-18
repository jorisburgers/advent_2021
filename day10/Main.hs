import Text.Parsec hiding (Line)
import Text.Parsec.String

import Data.Maybe (mapMaybe)
import Data.List (sort)

data SymbolType 
  = Smooth
  | Curly
  | Square
  | Pointy
  deriving (Eq, Show)

data Symbol
  = Open SymbolType
  | Close SymbolType
  deriving Show

pLines :: Parser [[Symbol]]
pLines = sepBy pLine endOfLine <* eof

pLine :: Parser [Symbol]
pLine = many pSymbol

pSymbol :: Parser Symbol
pSymbol 
  =   Open Smooth <$ char '('
  <|> Close Smooth <$ char ')'
  <|> Open Curly <$ char '{'
  <|> Close Curly <$ char '}'
  <|> Open Square <$ char '['
  <|> Close Square <$ char ']'
  <|> Open Pointy <$ char '<'
  <|> Close Pointy <$ char '>'

incomplete :: [Symbol] -> Maybe [SymbolType]
incomplete = go []
  where
    go :: [SymbolType] -> [Symbol] -> Maybe [SymbolType]
    go [] [] = Nothing
    go stack [] = Just stack
    go stack (Open x:xs) = go (x : stack) xs
    go [] (Close x: xs) = go [] xs -- This should not occur for valid cases
    go (s:ss) (Close x:xs) 
      | s == x = go ss xs
      | otherwise = Nothing -- This is a corrupt one, should fall in the case of corrupted

points :: SymbolType -> Int
points Smooth = 1
points Curly  = 3
points Square = 2
points Pointy = 4

makeScore :: [SymbolType] -> Int
makeScore = go 0
  where
    go n [] = n
    go n (x:xs) = go (n * 5 + points x) xs

main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pLines inputFile
  case res of 
    Left e -> print e
    Right lines -> do
      let scores = sort $  map makeScore (mapMaybe incomplete lines)
      print (scores !! (length scores `div` 2))
      
