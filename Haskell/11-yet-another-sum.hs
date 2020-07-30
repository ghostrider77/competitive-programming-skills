import Data.List (sortOn)


convertToIntList :: String -> [Int]
convertToIntList = map read . words


calculateSum :: [Int] -> Double
calculateSum array =
    let reciprocals = map (\x -> 1 / fromIntegral x) array
        positives = sortOn abs $ filter (> 0) reciprocals
        negatives = sortOn abs $ filter (< 0) reciprocals
    in (sum positives + sum negatives) + fromIntegral (sum array)


main :: IO()
main = do
    _ <- readLn :: IO Int
    array <- fmap convertToIntList getLine
    let result = calculateSum array
    print result
