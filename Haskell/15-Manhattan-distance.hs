import Control.Monad (replicateM)

data Point = Point { xCoord :: Int, yCoord :: Int, pointIx :: Int }
data Value = Value { value :: Int, index :: Int }


convertToIntList :: String -> [Int]
convertToIntList = map read . words


readPoints :: Int -> IO [Point]
readPoints n = do
    lines' <- replicateM n $ fmap convertToIntList getLine
    return $ map (\([x, y], ix) -> Point x y ix) $ zip lines' [1..]


maximalManhattanDistance :: [Point] -> [(Int, Int)]
maximalManhattanDistance points =
    let (Point x y ix) = head points
        go :: [Point] -> [(Int, Int)] -> Value -> Value -> Value -> Value -> [(Int, Int)]
        go [] acc _ _ _ _ = reverse acc
        go ((Point x y ix) : xss) acc maxSum maxDiff minSum minDiff =
            let maxSum' = if x + y > value maxSum then Value (x + y) ix else maxSum
                minSum' = if x + y < value minSum then Value (x + y) ix else minSum
                maxDiff' = if x - y > value maxDiff then Value (x - y) ix else maxDiff
                minDiff' = if x - y < value minDiff then Value (x - y) ix else minDiff
            in if (value maxSum' - value minSum' > value maxDiff' - value minDiff')
                then go xss ((index maxSum', index minSum') : acc) maxSum' maxDiff' minSum' minDiff'
                else go xss ((index maxDiff', index minDiff') : acc) maxSum' maxDiff' minSum' minDiff'
    in go (tail points) [(ix, ix)] (Value (x + y) ix) (Value (x - y) ix) (Value (x + y) ix) (Value (x - y) ix)


main :: IO()
main = do
    n <- readLn
    points <- readPoints n
    let result = maximalManhattanDistance points
    mapM_ (\(i, j) -> putStrLn $ unwords $ map show [i, j]) result
