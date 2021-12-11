import Data.Foldable (foldl')

import Text.Parsec
import Text.Parsec.String

data Command
  = Up Int
  | Down Int
  | Forward Int
  deriving Show

pCommand :: Parser (Int -> Command)
pCommand 
  =   Up <$ string "up" 
  <|> Down <$ string "down" 
  <|> Forward <$ string "forward"

pInstruction :: Parser Command
pInstruction = pCommand <* space <*> (read <$> many1 digit)

pInstructions :: Parser [Command]
pInstructions = many (pInstruction <* (() <$ endOfLine <|> eof) )

data SubState = SubState 
  { stateDepth :: Int
  , stateHorizontal :: Int
  }

emptyState :: SubState
emptyState = SubState 0 0 

executeCommand :: SubState -> Command -> SubState
executeCommand s (Up n) = s {stateDepth = stateDepth s - n}
executeCommand s (Down n) = s {stateDepth = stateDepth s + n}
executeCommand s (Forward n) = s {stateHorizontal = stateHorizontal s + n}

eval :: SubState -> [Command] -> SubState
eval s = foldl' executeCommand s

main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pInstructions inputFile
  case res of 
    Left e -> print e
    Right instr -> do
      let final = eval emptyState instr
      print (stateDepth final * stateHorizontal final)