import Control.Monad (replicateM)
import Data.Array (Array, listArray, range, (!))
import Data.List (sort)

data Item =  Item Int Int


convertToIntList :: String -> [Int]
convertToIntList = map read . words


readItems :: Int -> IO [Item]
readItems nrItems = replicateM nrItems $ fmap ((\[w, v] -> Item w v) . convertToIntList) getLine


findItemIndices :: Array (Int, Int) Int -> Array Int Item -> Int -> Int -> [Int]
findItemIndices knapsack items n capacity = go [] capacity n
    where
        go :: [Int] -> Int -> Int -> [Int]
        go indices currentCapacity k
            | k < 1 || currentCapacity <= 0 = indices
            | otherwise = let (Item weight _) = items ! k  in
                if (currentCapacity < weight) then go indices currentCapacity (k - 1)
                else
                    let previous = if k == 1 then 0 else knapsack ! (currentCapacity, k - 1)
                    in if knapsack ! (currentCapacity, k) /= previous
                        then go (k : indices) (currentCapacity - weight) (k - 1)
                        else go indices currentCapacity (k - 1)
            | otherwise = sort indices


solveKnapsackProblem :: [Item] -> Int -> Int -> [Int]
solveKnapsackProblem items nrItems capacity =
    let xs = listArray (1, nrItems) items
        solve :: Int -> Int -> Int
        solve c jy
            | c == 0 || jy == 0 = 0
            | otherwise = let Item weight value = xs ! jy in
                if c < weight then knapsack ! (c, jy - 1)
                else max (knapsack ! (c - weight, jy - 1) + value) (knapsack ! (c, jy - 1))
        bounds = ((0, 0), (capacity, nrItems))
        knapsack = listArray bounds [solve c k | (c, k) <- range bounds]
    in findItemIndices knapsack (listArray (1, nrItems) items) nrItems capacity


main :: IO()
main = do
    [nrItems, capacity] <- fmap convertToIntList getLine
    items <- readItems nrItems
    let result = solveKnapsackProblem items nrItems capacity
    print $ length result
    putStrLn $ unwords $ map show result
