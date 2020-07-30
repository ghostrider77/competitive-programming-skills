import Data.Array (Array, listArray, (!))
import Control.Monad (replicateM)
import Data.List (maximumBy)
import Data.Ord (comparing)


data Query = Query { left :: Int, right :: Int }

data SegmentTree =
    Leaf {ix :: Int, freqs :: [Int]} |
    Node {ix :: Int
    , leftEnd :: Int
    , rightEnd :: Int
    , leftChild :: SegmentTree
    , rightChild :: SegmentTree
    , freqs :: [Int]} deriving Show


alphabet :: [Char]
alphabet = ['a'..'z']


alphabetSize :: Int
alphabetSize = length alphabet


convertToIntList :: String -> [Int]
convertToIntList = map read . words


readQueries :: Int -> IO [Query]
readQueries nrQueries = do
    lines' <- replicateM nrQueries getLine
    return $ map (\line -> let [a, b] = convertToIntList line in Query a b) lines'


findChar :: [Int] -> Char
findChar frequencies = fst $ maximumBy (comparing snd) $ zip alphabet frequencies


combine :: [Int] -> [Int] -> [Int]
combine frequencies1 frequencies2 = map (\(f1, f2) -> f1 + f2) $ zip frequencies1 frequencies2


createSingleLetterFrequencyList :: Char -> [Int]
createSingleLetterFrequencyList char = map (\c -> if c == char then 1 else 0) alphabet


calcSegmentTree :: Array Int Char -> Int -> SegmentTree
calcSegmentTree chars n =
    let buildTree :: Int -> Int -> Int -> SegmentTree
        buildTree ix left right
            | left == right = Leaf left $ createSingleLetterFrequencyList $ chars ! left
            | otherwise =
                let ix' = 2 * ix + 1
                    ix'' = ix' + 1
                    middleIx = (left + right) `div` 2
                    leftChild = buildTree ix' left middleIx
                    rightChild = buildTree ix'' (middleIx + 1) right
                in Node ix left right leftChild rightChild $ combine (freqs leftChild) (freqs rightChild)
        in buildTree 0 0 (n - 1)


mostFrequentCharsInRange :: Array Int Char -> [Query] -> Int -> [Char]
mostFrequentCharsInRange chars queries n =
    let segmentTree = calcSegmentTree chars n
        getFrequenciesInSegment :: SegmentTree -> Int -> Int -> Int -> Int -> [Int]
        getFrequenciesInSegment tree leftEnd rightEnd i j
            | i <= leftEnd && j >= rightEnd = freqs tree
            | i > rightEnd || j < leftEnd = replicate n 0
            | otherwise =
                let middleIx = (leftEnd + rightEnd) `div` 2
                    frequencies1 = getFrequenciesInSegment (leftChild tree)  leftEnd middleIx i j
                    frequencies2 = getFrequenciesInSegment (rightChild tree) (middleIx + 1) rightEnd i j
                in combine frequencies1 frequencies2
    in map (\(Query a b) ->
        let freqs = getFrequenciesInSegment segmentTree 0 (n - 1) (a - 1) (b - 1) in findChar freqs) queries


main :: IO()
main = do
    string <- getLine
    let n = length string
    let chars = listArray (0, n - 1) string
    nrQueries <- readLn
    queries <- readQueries nrQueries
    let result = mostFrequentCharsInRange chars queries n
    mapM_ putStrLn $ map (: []) result
