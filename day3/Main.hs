
import Text.Parsec hiding (count)
import Text.Parsec.String

import qualified Data.Map as M

data Bit = Zero | One
  deriving Show

inv :: Bit -> Bit
inv Zero = One
inv One = Zero

data Byte = Byte { unByte :: [Bit]}
  deriving Show

toInt :: Byte -> Int
toInt (Byte bits) = fst $ foldr shift (0, 1) bits
  where
    shift :: Bit -> (Int, Int) -> (Int, Int)
    shift Zero (sum, pow) = (sum, pow * 2)
    shift One (sum, pow)  = (sum + pow, pow * 2)

pBit :: Parser Bit
pBit = One <$ char '1' <|> Zero <$ char '0'

pByte :: Parser Byte
pByte = Byte <$> many1 pBit <* (() <$ endOfLine <|> eof)

pBytes :: Parser [Byte]
pBytes = many pByte

data Count = Count 
  { countZero :: Int
  , countOne :: Int
  } deriving Show

instance Semigroup Count where
  c1 <> c2 = Count 
    { countZero = countZero c1 + countZero c2
    , countOne = countOne c1 + countOne c2
    }

getCount :: Bit -> Count
getCount Zero = Count 0 1
getCount One = Count 1 0

mostCommon :: Count -> Bit
mostCommon (Count zero one) 
  | zero > one = Zero
  | otherwise = One

count :: [Byte] -> M.Map Int Count
count = foldr count' M.empty
    where
      count' :: Byte -> M.Map Int Count -> M.Map Int Count
      count' (Byte bits) = flip (foldr countBit) (zip bits [0..])
      countBit :: (Bit, Int) -> M.Map Int Count -> M.Map Int Count
      countBit (bit, index) = M.insertWith (<>) index (getCount bit) 

epsilon :: [Count] -> Int
epsilon = toInt . Byte . map mostCommon

gamma :: [Count] -> Int
gamma = toInt . Byte . map (inv . mostCommon)

main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pBytes inputFile
  case res of 
    Left e -> print e
    Right bytes -> do
      let counted = count bytes
      let countedList = map (counted M.!) [0..(M.size counted - 1)]
      print (epsilon countedList * gamma countedList)