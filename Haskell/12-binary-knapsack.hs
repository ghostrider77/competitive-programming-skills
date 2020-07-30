import Control.Monad (replicateM)
import Data.List (partition, sortBy)

data Item = Item { weight :: Int, value :: Int }


convertToIntList :: String -> [Int]
convertToIntList = map read . words


readItems :: Int -> IO [Item]
readItems n = do
    lines' <- replicateM n getLine
    return $ map (\line -> let [w, v] = convertToIntList line in Item w v) lines'


mergeSmallestItems :: [Item] -> [Item]
mergeSmallestItems items =
    let sortedItems = sortBy (\(Item _ v1) (Item _ v2) -> compare v2 v1) items in go sortedItems []
    where
        go :: [Item] -> [Item] -> [Item]
        go [] merged = merged
        go [Item w v] merged = (Item 2 v) : merged
        go ((Item _ v1) : (Item _ v2) : xss) merged = go xss ((Item 2 (v1 + v2)) : merged)


findItemWithLargestValue :: [Item] -> (Int, [Item])
findItemWithLargestValue items =
    let sortedItems = sortBy (\(Item _ v1) (Item _ v2) -> compare v2 v1) items
    in (value $ head sortedItems, tail sortedItems)


solveBinaryKnapsack :: [Item] -> Int -> Int
solveBinaryKnapsack items capacity = go capacity 0 items
    where
        checkCapacityParity :: [Item] -> Int -> Int -> ([Item], Int, Int)
        checkCapacityParity smallestItems c v
            | even c = (mergeSmallestItems smallestItems, c, v)
            | not $ null smallestItems =
                let (value, remainingItems) = findItemWithLargestValue smallestItems
                in (mergeSmallestItems remainingItems, c - 1, v + value)
            | otherwise = (smallestItems, c - 1, v)
        go :: Int -> Int -> [Item] -> Int
        go currentCapacity totalValue currentItems
            | currentCapacity <= 0 = totalValue
            | otherwise =
                let (smallestItems, rest) = partition (\(Item w _) -> w == 1) currentItems
                    (mergedItems, capacity', value') = checkCapacityParity smallestItems currentCapacity totalValue
                    items' = map (\(Item w v) -> Item (w `div` 2) v) (rest ++ mergedItems)
                in go (capacity' `div` 2) value' items'


main :: IO()
main = do
    [n, capacity] <- fmap convertToIntList getLine
    items <- readItems n
    let result = solveBinaryKnapsack items capacity
    print result
