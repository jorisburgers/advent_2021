import Text.Parsec hiding (Line)
import Text.Parsec.String

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

corrupted :: [Symbol] -> Maybe SymbolType
corrupted = go []
  where
    go :: [SymbolType] -> [Symbol] -> Maybe SymbolType
    go stack [] = Nothing
    go stack (Open x:xs) = go (x : stack) xs
    go [] (Close x: xs) = go [] xs -- This should not occur for valid cases
    go (s:ss) (Close x:xs) 
      | s == x = go ss xs
      | otherwise = Just x

points :: SymbolType -> Int
points Smooth = 3
points Curly  = 1197
points Square = 57
points Pointy = 25137
 
main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pLines inputFile
  case res of 
    Left e -> print e
    Right lines -> do
      print (sum $ map (maybe 0 points . corrupted) lines)
      
