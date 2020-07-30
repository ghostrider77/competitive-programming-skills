convertToIntList :: String -> [Int]
convertToIntList = map read . words


findElemInSequence :: Int -> Int -> Int -> Int
findElemInSequence y difference z =
    let (q1, r1) = divMod (z  -y) difference
    in if r1 == 0 && q1 > 0 then 2*q1 - 1
        else let (q2, r2) = divMod z difference
             in if (r2 == 0 && q2 > 0) then 2*q2 else -1


findFirstOccurrenceInSequence :: Int -> Int -> Int -> Int
findFirstOccurrenceInSequence x y z
    | z == 0 = 0
    | z == x = 1
    | otherwise =
        let difference = x - y
        in if difference == 0 && z /= x then -1 else findElemInSequence y difference z


main :: IO()
main = do
    [x, y, z] <- fmap convertToIntList getLine
    print $ findFirstOccurrenceInSequence x y z
