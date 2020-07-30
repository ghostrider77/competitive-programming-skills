convertToIntList :: String -> [Int]
convertToIntList = map read . words


findArgmax :: [Int] -> Int -> Int -> Int
findArgmax array maxValue count =
    let maxIndices = snd $ unzip $ filter (\(x, _) -> x == maxValue) $ zip array [0..]
    in if count >= 3 then maxIndices !! 2 else head maxIndices


removeMaximum :: [Int] -> [Int]
removeMaximum array =
    let maxValue = maximum array
        count = foldl (\acc x -> if x == maxValue then acc + 1 else acc) 0 array
        maxIx = findArgmax array maxValue count
    in fst $ unzip $ filter (\(_, ix) -> ix /= maxIx) $ zip array [0..]


main :: IO()
main = do
    _ <- readLn :: IO Int
    array <- fmap convertToIntList getLine
    let result = removeMaximum array
    putStrLn $ unwords $ map show result
