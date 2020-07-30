import Data.Array (Array, listArray, range, (!))

data Penalties = Penalties Int Int Int


convertToIntList :: String -> [Int]
convertToIntList = map read . words


calcEditDistance :: String -> String -> Int -> Int -> Penalties -> Int
calcEditDistance s1 s2 n m penalties =
    let Penalties ip dp sp = penalties
        (a1, a2) = (listArray (1, n) s1, listArray (1, m) s2)
        editDistance :: Int -> Int -> Int
        editDistance ix 0 = ix * dp
        editDistance 0 jy = jy * ip
        editDistance ix jy =
            let matchingScore = if (a1 ! ix == a2 ! jy) then 0 else sp
                deletion = editDistanceValues ! (ix - 1, jy) + dp
                insertion = editDistanceValues ! (ix, jy - 1) + ip
                matching = editDistanceValues ! (ix - 1, jy - 1) + matchingScore
            in minimum [deletion, insertion, matching]
        bounds = ((0, 0), (n, m))
        editDistanceValues = listArray bounds [editDistance ix jy | (ix, jy) <- range bounds]
    in editDistance n m


main :: IO()
main = do
    [n, m] <- fmap convertToIntList getLine
    s1 <- getLine
    s2 <- getLine
    penalties <- fmap ((\[ip, dp, sp] -> Penalties ip dp sp) . convertToIntList) getLine
    let result = calcEditDistance s1 s2 n m penalties
    print result
