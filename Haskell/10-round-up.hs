convertToIntList :: String -> [Int]
convertToIntList = map read . words


main :: IO()
main = do
    [x, y] <- fmap convertToIntList getLine
    let result = if x `mod` y == 0 then x `div` y else ceiling (fromIntegral x / fromIntegral y)
    print result
