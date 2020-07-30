import Control.Monad (replicateM)
import Data.Array (Array, listArray, range, (!))


convertToIntList :: String -> [Int]
convertToIntList = map read . words


backtrackLCS :: Array (Int, Int) Int -> Int -> ([Int], [Int])
backtrackLCS backtrack n = go [] [] n n
    where
        go :: [Int] -> [Int] -> Int -> Int -> ([Int], [Int])
        go ixs1 ixs2 i j
            | i <= 0 || j <= 0 = (ixs1, ixs2)
            | backtrack ! (i, j) == 0 = go ((i - 1) : ixs1) ((j - 1) : ixs2) (i - 1) (j - 1)
            | backtrack ! (i, j) == -1 = go ixs1 ixs2 (i - 1) j
            | otherwise = go ixs1 ixs2 i (j - 1)


longestCommonSubsequence :: Array Int Int -> Array Int Int -> Int -> ([Int], [Int])
longestCommonSubsequence s1 s2 n =
    let fillTables :: Int -> Int -> (Int, Int)
        fillTables 0 _ = (0, 0)
        fillTables _ 0 = (0, 0)
        fillTables i j =
            let mismatch = if s1 ! (i-1) == s2 ! (j-1) then 1 else 0
                path = maximum [longestPath ! (i-1, j), longestPath ! (i, j-1), longestPath ! (i-1, j-1) + mismatch]
                direction
                    | path == longestPath ! (i-1, j) = -1
                    | path == longestPath ! (i, j-1) = 1
                    | otherwise = 0
            in (path, direction)
        bounds = ((0, 0), (n, n))
        (longestPath, backtrack) =
            let (lp, bt) = unzip $ [fillTables i j | (i, j) <- range bounds]
            in (listArray bounds lp, listArray bounds bt)
    in backtrackLCS backtrack n


main :: IO()
main = do
    n <- readLn
    s1 <- fmap (listArray (0, n - 1) . convertToIntList) getLine
    s2 <- fmap (listArray (0, n - 1) . convertToIntList) getLine
    let (ixs1, ixs2) = longestCommonSubsequence s1 s2 n
    print $ length ixs1
    putStrLn $ unwords $ map show ixs1
    putStrLn $ unwords $ map show ixs2
