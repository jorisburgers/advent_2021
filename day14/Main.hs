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

expand :: M.Map (Letter, Letter) Letter -> M.Map (Letter, Letter) Int -> M.Map (Letter, Letter) Int
expand rules hist = foldr join M.empty (M.toList hist)
  where
    join :: ((Letter, Letter), Int) -> M.Map (Letter, Letter) Int -> M.Map (Letter, Letter) Int
    join (pair@(x, y), c) h = case M.lookup pair rules of
      Nothing -> M.insertWith (+) pair c $! h
      Just z -> M.insertWith (+) (x, z) c $! M.insertWith (+) (z, y) c $! h

expandN :: Int -> M.Map (Letter, Letter) Letter -> M.Map (Letter, Letter) Int -> M.Map (Letter, Letter) Int
expandN 0 _ ls = ls
expandN n rules ls = expandN (n - 1) rules (expand rules ls)

histogram :: [Letter] -> M.Map (Letter, Letter) Int
histogram (l1:l2:ls) = M.insertWith (+) (l1, l2) 1 $ histogram (l2:ls)
histogram _ = M.empty

frequencies :: M.Map (Letter, Letter) Int -> M.Map Letter Int
frequencies = M.map (\x -> (x + 1) `div` 2 ) . M.fromListWith (+) . concatMap f . M.toList 
  where
    f :: ((Letter, Letter), Int) -> [(Letter, Int)]
    f ((x, y), c) = [(x, c), (y, c)]

main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pFormulaWithRules inputFile
  case res of 
    Left e -> print e
    Right (formula, rules) -> do
      let iterN = expandN 40 rules (histogram formula)
      let hist = M.toList $ frequencies iterN
      let (_, min) = minimumBy (compare `on` snd) hist
      let (_, max) = maximumBy (compare `on` snd) hist
      print (max - min)