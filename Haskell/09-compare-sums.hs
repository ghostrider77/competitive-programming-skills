convertToDoubleList :: String -> [Double]
convertToDoubleList = map read . words


compareSum :: [Double] -> [Double] -> String
compareSum array1 array2 =
    let s1 = sum array1
        s2 = sum array2
    in if abs (s1 - s2) < 1e-6 then "SUM(A)=SUM(B)"
       else if s1 < s2 then "SUM(A)<SUM(B)"
       else "SUM(A)>SUM(B)"


main :: IO()
main = do
    _ <- readLn :: IO Int
    array1 <- fmap convertToDoubleList getLine
    array2 <- fmap convertToDoubleList getLine
    let result = compareSum array1 array2
    putStrLn result
