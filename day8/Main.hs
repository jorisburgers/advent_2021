import Text.Parsec hiding (Line)
import Text.Parsec.String

import qualified Data.Map as M

data Line = Line [String] [String]
  deriving Show

pSegment :: Parser String 
pSegment = many1 (oneOf "abcdefg")

pSegments :: Parser [String]
pSegments = sepEndBy pSegment (char ' ')

pLine :: Parser Line
pLine = Line <$> pSegments <* string "| " <*> pSegments

pLines :: Parser [Line]
pLines = sepBy pLine endOfLine <* eof

countSndLengths :: [Int] -> Line -> Int
countSndLengths ls (Line _ xs) = length (filter (\x -> length x `elem` ls) xs)

main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pLines inputFile
  case res of 
    Left e -> print e
    Right lines -> do
      print (sum (map (countSndLengths [2, 3, 4, 7]) lines))
