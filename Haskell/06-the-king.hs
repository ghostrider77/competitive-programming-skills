convertToIntList :: String -> [Int]
convertToIntList = map read . words


maximumNumberOfKings :: Int -> Int -> Int
maximumNumberOfKings rows cols =
    let emptyCells = ceiling (fromIntegral rows / 3) * ceiling (fromIntegral cols / 3)
    in rows * cols - emptyCells


main :: IO()
main = do
    [rows, cols] <- fmap convertToIntList getLine
    let result = maximumNumberOfKings rows cols
    print result
