
-- | Depth we are at
-- We don't allow Num for depth, adding or subtracting depth makes no sense in this case, so lets not allow that.
newtype Depth = Depth {unDepth :: Integer}
  deriving (Eq, Ord, Show)

depthIncrease :: [Depth] -> Integer
depthIncrease = go 0
  where
    go total [] = total
    go total (m:[]) = total
    go total (x:y:ys) 
      | x < y = go (total + 1) (y:ys)
      | otherwise = go total (y:ys)

main :: IO ()
main = do
  let inputFile = "input"
  lines <- lines <$> readFile inputFile
  let depths = map (Depth . read) lines
  print (depthIncrease depths)

