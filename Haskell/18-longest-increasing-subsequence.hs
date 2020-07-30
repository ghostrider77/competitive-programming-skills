import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as M
import Data.List (find)


convertToIntList :: String -> [Int]
convertToIntList = map read . words


longestIncreasingSubsequence :: [Int] -> Int
longestIncreasingSubsequence array = go array M.empty 0
    where
        go :: [Int] -> IntMap [Int] -> Int -> Int
        go [] _ nrStacks = nrStacks
        go (x : xs) stacks nrStacks =
            case find (\ix -> x <= head (stacks ! ix)) [0..(nrStacks-1)] of
                Nothing -> go xs (M.insert nrStacks [x] stacks) (nrStacks + 1)
                Just ix -> go xs (M.adjust (x :) ix stacks) nrStacks


main :: IO()
main = do
    _ <- readLn :: IO Int
    array <- fmap convertToIntList getLine
    let result = longestIncreasingSubsequence array
    print result
