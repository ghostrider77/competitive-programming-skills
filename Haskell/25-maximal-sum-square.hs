import Control.Monad (replicateM)
import Data.Array (Array, listArray, range, (!))


convertToIntList :: String -> [Int]
convertToIntList = map read . words


readMatrix :: Int -> IO (Array (Int, Int) Int)
readMatrix n = do
    rows <- replicateM n $ fmap convertToIntList getLine
    return $ listArray ((0, 0), (n - 1, n - 1)) [x | row <- rows, x <- row]


maximalSumSquareMatrix :: Array (Int, Int) Int -> Int -> Int -> Int
maximalSumSquareMatrix matrix n k =
    let fillPrefixSum :: Int -> Int -> Int
        fillPrefixSum 0 _ = 0
        fillPrefixSum _ 0 = 0
        fillPrefixSum i j = prefixSum ! (i-1, j) + prefixSum ! (i, j-1) - prefixSum ! (i-1, j-1) + matrix ! (i-1, j-1)
        bounds = ((0, 0), (n, n))
        prefixSum = listArray bounds [fillPrefixSum i j | (i, j) <- range bounds]
    in foldl (\acc i ->
        foldl (\inner j ->
            let ps = prefixSum ! (i+k, j+k) - prefixSum ! (i+k, j) - prefixSum ! (i, j+k) + prefixSum ! (i, j)
            in max inner ps) acc [0..(n-k)] ) 0 [0..(n-k)]


main :: IO()
main = do
    [n, k] <- fmap convertToIntList getLine
    matrix <- readMatrix n
    let result = maximalSumSquareMatrix matrix n k
    print result
