convertToIntList :: String -> [Int]
convertToIntList = map read . words


sumOfMinimums :: [Int] -> Int
sumOfMinimums array = go array 0
    where
        go :: [Int] -> Int -> Int
        go [] acc = acc
        go xs @ (_ : xss) acc =
            let (_, s) = foldl (\(m, s) item -> let m' = if item < m then item else m in (m', s + m')) (maxBound, 0) xs
            in go xss (acc + s)


main :: IO()
main = do
    _ <- readLn :: IO Int
    array <- fmap convertToIntList getLine
    let result = sumOfMinimums array
    print result
