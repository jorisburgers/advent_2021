
import Text.Parsec hiding (count)
import Text.Parsec.String

import qualified Data.Map as M

data Bit = Zero | One
  deriving (Show, Eq)

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
  | zero >= one = Zero
  | otherwise = One

count :: [Byte] -> M.Map Int Count
count = foldr count' M.empty
    where
      count' :: Byte -> M.Map Int Count -> M.Map Int Count
      count' (Byte bits) = flip (foldr countBit) (zip bits [0..])
      countBit :: (Bit, Int) -> M.Map Int Count -> M.Map Int Count
      countBit (bit, index) = M.insertWith (<>) index (getCount bit) 

countFilter :: (Int -> Count -> Byte -> Bool) -> [Byte] -> Byte
countFilter f = go 0
  where
    go index [] = error "Should have a non-empty list"
    go index (x:[]) = x
    go index bytes = 
      let counted = count bytes 
      in go (index + 1) (filter (f index (counted M.! index)) bytes)
      

-- Normally !! is unsafe and inefficient, but for these short cases, we don't care about safety and efficiency
oxygen :: [Byte] -> Byte
oxygen = countFilter (\index count (Byte byte) -> byte !! index == inv (mostCommon count))

carbondioxide :: [Byte] -> Byte
carbondioxide = countFilter (\index count (Byte byte) -> byte !! index == mostCommon count)

main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pBytes inputFile
  case res of 
    Left e -> print e
    Right bytes -> do
      print (toInt (oxygen bytes) * toInt (carbondioxide bytes))