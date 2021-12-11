
-- | Depth we are at
-- We don't allow Num for depth, adding or subtracting depth makes no sense in this case, so lets not allow that.
newtype Depth = Depth {unDepth :: Integer}
  deriving (Eq, Ord, Show)

depthSliceValue :: [Depth] -> Depth
depthSliceValue = Depth . sum . map unDepth 

depthIncrease :: Int -> [Depth] -> Integer
depthIncrease compareSize = go 0
  where
    go total [] = total
    go total ms | length (take compareSize ms) < compareSize = total
    go total ms =
      let curSlice = depthSliceValue (take compareSize ms)
          nextSlice = depthSliceValue (take compareSize (drop 1 ms))
          newTotal = if curSlice < nextSlice then total + 1 else total
      in go newTotal (drop 1 ms)

main :: IO ()
main = do
  let inputFile = "input"
  lines <- lines <$> readFile inputFile
  let depths = map (Depth . read) lines
  print (depthIncrease 3 depths)

