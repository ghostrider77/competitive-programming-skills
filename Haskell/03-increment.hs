incrementLength :: String -> Int
incrementLength digits = if all (== '9') digits then length digits + 1 else length digits


main :: IO()
main = do
    digits <- getLine
    let result = incrementLength digits
    print result
