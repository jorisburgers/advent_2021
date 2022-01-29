

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

type NodeValue = (Maybe (Int, Int), Weight, Bool)

shortestPath :: Board -> M.Map (Int, Int) NodeValue
shortestPath board = go init
  where
    go ::  M.Map (Int, Int) NodeValue -> M.Map (Int, Int) NodeValue
    go b = case smallest b of
      Nothing -> b
      Just (k, w) -> 
        let ns = map (\k -> (k, M.lookup k board)) $ neighbours k
        in go $ M.adjust markVisited k $ foldr (updateNeighbours k w) b ns
      
    updateNeighbours :: (Int, Int) -> Int -> ((Int, Int), Maybe Int) -> M.Map (Int, Int) NodeValue  -> M.Map (Int, Int) NodeValue
    updateNeighbours _ _ (_, Nothing) b = b
    updateNeighbours origK distK (n, Just w) b =
      let alt = distK + w in M.adjust (\nDist@(prev, dist, visited) -> if Number alt < dist then (Just origK, Number alt, visited) else nDist) n b

    markVisited :: NodeValue -> NodeValue
    markVisited (x, y, False) = (x, y, True)
        

    init :: M.Map (Int, Int) NodeValue
    init = M.insert (0, 0) (Nothing, Number 0, False) $ M.map (\_ -> (Nothing, Infinity, False)) board

    smallest :: M.Map (Int, Int) NodeValue -> Maybe ((Int, Int), Int) 
    smallest = M.foldrWithKey join Nothing
      where
        join :: (Int, Int) -> (a, Weight, Bool) -> Maybe ((Int, Int), Int) -> Maybe ((Int, Int), Int) 
        join _ (_, _, True) x = x
        join _ (_, Infinity, _) Nothing = Nothing
        join k (_, Number w, _) Nothing = Just (k, w)
        join k (_, Infinity, _) (Just x) = Just x
        join k (_, Number w, _) (Just (k', w')) = if w < w' then Just (k, w) else Just (k', w')



main :: IO ()
main = do
  let inputFile = "input"
  res <- parseFromFile pBoard inputFile
  case res of 
    Left e -> print e
    Right board -> do
      let shortest = shortestPath $ mkBoard board
      let (k, v) = M.findMax shortest
      let (prev, length, _visited) = v
      print length