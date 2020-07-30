convertToIntList :: String -> [Int]
convertToIntList = map read . words


makeArraySorted :: [Int] -> Int
makeArraySorted array=
    let limit = 1000
        firstItem = head array
        firstRow = map (\x -> abs (x - firstItem)) [1..limit]
        processArray :: [Int] -> Int -> [Int]
        processArray previousRow item =
            let createNextRow :: (Int, [Int]) -> (Int, Int) -> (Int, [Int])
                createNextRow (m, acc) (itemInPreviousRow, y) =
                    let m' = min m itemInPreviousRow
                    in (m', (m' + abs (y - item)) : acc)
            in reverse $ snd $ foldl createNextRow (head previousRow, []) $ zip previousRow [1..limit]
    in minimum $ foldl processArray firstRow $ tail array


main :: IO()
main = do
    _ <- readLn :: IO Int
    array <- fmap convertToIntList getLine
    let result = makeArraySorted array
    print result
