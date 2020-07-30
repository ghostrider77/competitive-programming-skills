import Data.Map.Strict (Map, fromList, (!))


charToFunc :: Map Char (Int -> Int -> Int)
charToFunc = fromList [('+', (+)), ('-', (-))]


splitString :: String -> ([Int], [Char])
splitString string = go string [] [] []
    where
        go :: [Char] -> [Int] -> [Char] -> [Char] -> ([Int], [Char])
        go [] numbers ops currentNumber = (reverse $ (read $ reverse currentNumber) : numbers, reverse ops)
        go (x : xss) numbers ops currentNumber
            | x == '+' || x == '-' = let n = read $ reverse currentNumber in go xss (n : numbers) (x : ops) []
            | otherwise = go xss numbers ops (x : currentNumber)


calculate :: [Int] -> [Char] -> Int
calculate numbers operations =
    foldl (\acc (n, op) -> (charToFunc ! op) acc n) (head numbers) $ zip (tail numbers) operations


main :: IO()
main = do
    string <- getLine
    let (numbers, operations) = splitString string
    let result = calculate numbers operations
    print result
