import Data.Function (on)
import Data.List (minimumBy, maximumBy)
import Text.Parsec hiding (Line)
import Text.Parsec.String


import qualified Data.Map as M

newtype Letter = Letter Char
  deriving (Eq, Ord)

instance Show Letter where
  show (Letter x) = [x]

pFormulaWithRules :: Parser ([Letter], M.Map (Letter, Letter) Letter)
pFormulaWithRules = (,) <$> pFormula <* endOfLine <*> pRules

pFormula :: Parser [Letter]
pFormula = many1 pLetter <* endOfLine

pRules :: Parser (M.Map (Letter, Letter) Letter)
pRules = M.fromList <$> sepBy pRule endOfLine <* eof

pRule :: Parser ((Letter, Letter), Letter)
pRule = (\c1 c2 c3 -> ((c1, c2), c3)) <$> pLetter <*> pLetter <* string " -> " <*> pLetter

pLetter :: Parser Letter
pLetter = Letter <$> letter 

expand :: M.Map (Letter, Letter) Letter -> [Letter] -> [Letter]
expand rules [] = []
expand rules [l] = [l]
expand rules (l1:l2:ls) = new <> expand rules (l2:ls)
  where
    new = maybe [l1] (\l' -> [l1, l']) $ M.lookup (l1, l2) rules

expandN :: Int -> M.Map (Letter, Letter) Letter -> [Letter] -> [Letter]
expandN 0 _ ls = ls
expandN n rules ls = expandN (n - 1) rules (expand rules ls)

histogram :: [Letter] -> M.Map Letter Int
histogram [] = M.empty
histogram (l:ls) = M.insertWith (+) l 1 $ histogram ls

main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pFormulaWithRules inputFile
  case res of 
    Left e -> print e
    Right (formula, rules) -> do
      let iter10 = expandN 10 rules formula
      let hist = M.toList $ histogram iter10
      let (_, min) = minimumBy (compare `on` snd) hist
      let (_, max) = maximumBy (compare `on` snd) hist
      print (max - min)