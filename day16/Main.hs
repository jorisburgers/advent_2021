import Text.Parsec hiding (Line)

import Data.Bits 

data Packet = Packet 
  { packetVersion :: Int
  , packetType :: Int
  , packetValue :: SubPacket
  }
  deriving Show

data SubPacket
  = Literal Int
  | Operator [Packet]
  deriving Show

expandHex :: Char -> [Int]
expandHex '0' = [0, 0, 0, 0]
expandHex '1' = [0, 0, 0, 1]
expandHex '2' = [0, 0, 1, 0]
expandHex '3' = [0, 0, 1, 1]
expandHex '4' = [0, 1, 0, 0]
expandHex '5' = [0, 1, 0, 1]
expandHex '6' = [0, 1, 1, 0]
expandHex '7' = [0, 1, 1, 1]
expandHex '8' = [1, 0, 0, 0]
expandHex '9' = [1, 0, 0, 1]
expandHex 'A' = [1, 0, 1, 0]
expandHex 'B' = [1, 0, 1, 1]
expandHex 'C' = [1, 1, 0, 0]
expandHex 'D' = [1, 1, 0, 1]
expandHex 'E' = [1, 1, 1, 0]
expandHex 'F' = [1, 1, 1, 1]
expandHex _ = error "Found a non-exadecimal value"

type Parser = Parsec [Int] ()

pFixed :: Int -> Parser Int
pFixed len = go len 0 
  where
    go :: Int -> Int -> Parser Int
    go 0 x = pure x
    go n x = anyToken >>= \c -> go (n - 1) ((x `shiftL` 1) + c)

pPacket :: Parser (Packet, Int)
pPacket = do
  version <- pVersion
  typ <- pType
  (subPacket, len) <- case typ of
    4 -> pLiteral
    _ -> pOperator
  pure (Packet version typ subPacket, 3 + 3 + len)

pVersion :: Parser Int
pVersion = pFixed 3

pType :: Parser Int
pType = pFixed 3

pLiteral :: Parser (SubPacket, Int)
pLiteral = (\(sp, l) -> (Literal sp, l)) <$> pBlock 0

pBlock :: Int -> Parser (Int, Int)
pBlock x = pFixed 1 >>= \b ->
  case b of
    0 -> (\c -> ((x `shiftL` 4) + c, 5)) <$> pFixed 4 
    1 -> pFixed 4 >>= \c -> pBlock ((x `shiftL` 4) + c) >>= \(c, len) -> pure (c, len + 5)

pOperator :: Parser (SubPacket, Int)
pOperator = pFixed 1 >>= \b ->
  case b of
    0 -> do
      len <- pFixed 15
      let go n = if n <= 0 then pure [] else pPacket >>= \(p, l) -> ((p :) <$> go (n - l))
      packets <- go len
      pure (Operator packets, 1 + 15 + len)
    1 -> do
      len <- pFixed 11
      let go n = if n <= 0 then pure ([], 0) else do
            (p1, l1) <- pPacket 
            (ps, l2) <- go (n - 1)
            pure (p1 : ps, l1 + l2)
      (packets, pLen) <- go len
      pure (Operator packets, 1 + 11 + pLen)

sumVersion :: Packet -> Int
sumVersion p = packetVersion p + sumSub (packetValue p)
  where
    sumSub :: SubPacket -> Int
    sumSub (Literal{}) = 0
    sumSub (Operator packets) = sum (map sumVersion packets) 

eval :: Packet -> Int
eval (Packet version 0 (Operator ss)) = sum (map eval ss)
eval (Packet version 1 (Operator ss)) = product (map eval ss)
eval (Packet version 2 (Operator ss)) = minimum (map eval ss)
eval (Packet version 3 (Operator ss)) = maximum (map eval ss)
eval (Packet version 4 (Literal l)) = l
eval (Packet version 5 (Operator [s1, s2])) = if eval s1 > eval s2 then 1 else 0
eval (Packet version 6 (Operator [s1, s2])) = if eval s1 < eval s2 then 1 else 0
eval (Packet version 7 (Operator [s1, s2])) = if eval s1 == eval s2 then 1 else 0

main :: IO ()
main = do
  let inputFile = "input"
  input <- readFile inputFile
  let input' = concatMap expandHex input
  let res = runParser pPacket () inputFile input'
  case res of 
    Left e -> print e
    Right (packet, _bits_parsed) -> do
      print (eval packet)