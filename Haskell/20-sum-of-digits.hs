import Data.Array (Array, listArray, range, (!))

convertToIntList :: String -> [Int]
convertToIntList = map read . words


solveSumOfDigits :: Int -> Int -> Int
solveSumOfDigits n size
    | size == 1 = if 0 <= n && n <= 9 then 1 else 0
    | otherwise =
        let fillTable :: Int -> Int -> Int
            fillTable ix 0 = 0
            fillTable ix 1 = if 0 <= ix && ix <= min n 9 then 1 else 0
            fillTable 0 jy = 1
            fillTable ix jy = sum $ map (\k -> table ! (ix - k, jy - 1)) $ filter (ix >=) [0..9]
            bounds = ((0, 0), (n - 1, size - 1))
            table = listArray bounds [fillTable ix jy | (ix, jy) <- range bounds]
        in sum $ map (\k -> table ! (n - k, size - 1)) $ filter (n >=) [1..9]


main :: IO()
main = do
    [n, size] <- fmap convertToIntList getLine
    let result = solveSumOfDigits n size
    print result
