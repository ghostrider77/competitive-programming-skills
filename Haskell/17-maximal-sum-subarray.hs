convertToIntList :: String -> [Int]
convertToIntList = map read . words


calcPrefixesAndSuffixes :: [Int] -> ([Int], [Int])
calcPrefixesAndSuffixes array =
    let processMinPrefixSum :: (Int, [Int]) -> Int -> (Int, [Int])
        processMinPrefixSum (ps, acc @ (h : _)) x = let ps' = ps + x in (ps', (min h ps') : acc)
        minPrefixSum = reverse $ snd $ foldl processMinPrefixSum (0, [0]) array
        minSuffixSum = snd $ foldl processMinPrefixSum (0, [0]) $ reverse array
    in (minPrefixSum, minSuffixSum)


calcMaximalSumSubarrays :: [Int] -> Int -> [Int]
calcMaximalSumSubarrays array n =
    let (minPrefixSum, minSuffixSum) = calcPrefixesAndSuffixes array
        s = sum array
        minSums = zip3 (tail minSuffixSum) minPrefixSum [0..]
    in map (\(x, y, ix) -> if ix == 0 then s - x else if ix == n - 1 then s - y else s - (x + y)) minSums


main :: IO()
main = do
    n <- readLn
    array <- fmap convertToIntList getLine
    let result = calcMaximalSumSubarrays array n
    putStrLn $ unwords $ map show result
