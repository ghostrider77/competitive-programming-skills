import Control.Monad (replicateM)
import Data.Array (Array, listArray, (!))
import Data.List (permutations)


convertToIntArray :: Int -> String -> Array Int Int
convertToIntArray n string = listArray (0, n - 1) $ map read $ words string


readMatrix :: Int -> IO (Array Int (Array Int Int))
readMatrix n = do
    lines' <- replicateM n getLine
    return $ listArray (0, n - 1) $ map (convertToIntArray n) lines'


findCheapestPermutation :: (Array Int (Array Int Int)) -> Int -> [Int]
findCheapestPermutation matrix n =
    let xs = [0..(n-1)]
        calcCost :: [Int] -> Int
        calcCost perm = foldl (\cost (i, j) -> cost + matrix ! i ! j) 0 $ zip perm (tail perm)
        minPerm = fst $ foldl (\acc @ (_, minCost) p ->
            let cost = calcCost p in if cost < minCost then (p, cost) else acc) (xs, calcCost xs) $ permutations xs
    in if n == 1 then xs else minPerm


main :: IO()
main = do
    n <- readLn
    matrix <- readMatrix n
    let result = findCheapestPermutation matrix n
    putStrLn $ unwords $ map (show . (+ 1)) result
