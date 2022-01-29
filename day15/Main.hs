

import Text.Parsec hiding (Line)
import Text.Parsec.String

import qualified Data.Map as M

type Board = M.Map (Int, Int) Int

pBoard :: Parser [[Int]]
pBoard = sepBy1 pLine endOfLine <* eof

pLine :: Parser [Int]
pLine = many1 ((read . (:[])) <$> digit)

mkBoard :: [[Int]] -> Board
mkBoard doubleList = M.fromList $ concat $ zipWith (\line y -> zipWith (\i x -> ((x, y), i)) line [0..]) doubleList [0..]

expand :: Int -> [[Int]] -> [[Int]]
expand mul b = let
  upFactor :: Int -> Int -> Int
  upFactor factor v
    | v + factor > 9 = v + factor - 9
    | otherwise = v + factor



  high :: [[Int]]
  high = map (\row -> concatMap (\factor -> map (upFactor (factor :: Int)) (row :: [Int])) [0..mul-1]) b
  in concatMap (\factor -> map (map (upFactor factor)) high) [0..mul-1]

data Weight 
  = Number Int
  | Infinity
  deriving (Show, Eq)

instance Ord Weight where
  (Number x) <= (Number y) = x <= y
  Number{} <= Infinity = True
  Infinity <= Number{} = False
  Infinity <= Infinity = True

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

type NodeValue = (Weight, Bool)

shortestPath :: Board -> M.Map (Int, Int) Weight
shortestPath board = go init
  where
    maxCoord :: (Int, Int)
    maxCoord = fst $ M.findMax board

    go ::  M.Map (Int, Int) Weight -> M.Map (Int, Int) Weight
    go b = case smallest b of
      Nothing -> b
      Just (k, w) -> 
        let next = if k == maxCoord then id else (go . M.delete k)
            ns = map (\k -> (k, M.lookup k board)) $ neighbours k
        in next $ foldr (updateNeighbours w) b ns
      
    updateNeighbours :: Int -> ((Int, Int), Maybe Int) -> M.Map (Int, Int) Weight  -> M.Map (Int, Int) Weight
    updateNeighbours _ (_, Nothing) b = b
    updateNeighbours distK (n, Just w) b =
      let alt = distK + w in M.adjust (\dist -> if Number alt < dist then Number alt else dist) n b

        

    init :: M.Map (Int, Int) Weight
    init = M.insert (0, 0) (Number 0) $ M.map (\_ -> Infinity) board

    smallest :: M.Map (Int, Int) Weight -> Maybe ((Int, Int), Int) 
    smallest = M.foldrWithKey join Nothing
      where
        join :: (Int, Int) -> Weight -> Maybe ((Int, Int), Int) -> Maybe ((Int, Int), Int) 
        join _ Infinity Nothing = Nothing
        join k (Number w) Nothing = Just (k, w)
        join k Infinity (Just x) = Just x
        join k (Number w) (Just (k', w')) = if w < w' then Just (k, w) else Just (k', w')



main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pBoard inputFile
  case res of 
    Left e -> print e
    Right board -> do
      -- print $ mkBoard $ expand 5 $ [[8]]
      let lb = mkBoard $ expand 5 $ board
      print (length $ M.keys lb)
      let shortest = shortestPath lb
      let (k, v) = M.findMax shortest
      print v