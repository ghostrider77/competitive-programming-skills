import Control.Monad (replicateM)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

data Interval = Interval { left :: Int, right :: Int }


convertToIntList :: String -> [Int]
convertToIntList = map read . words


readIntervals :: Int -> IO [Interval]
readIntervals n = do
    lines' <- replicateM n $ fmap convertToIntList getLine
    return $ map (\[a, b] -> Interval a b) lines'


calcIntervalEndpointCounts :: [Interval] -> Int -> (IntMap Int, IntMap Int)
calcIntervalEndpointCounts intervals smallest = go intervals (M.empty, M.empty)
    where
        go :: [Interval] -> (IntMap Int, IntMap Int) -> (IntMap Int, IntMap Int)
        go [] acc = acc
        go ((Interval a b) : rest) (started, finished) =
            let started' = M.insertWith (+) (a - smallest) 1 started
                finished' = M.insertWith (+) (b - smallest) 1 finished
            in go rest (started', finished')


calcCounts :: IntMap Int -> IntMap Int -> Int -> [Int]
calcCounts started finished size = go [M.findWithDefault 0 0 started] 1
    where
        go :: [Int] -> Int -> [Int]
        go counts ix
            | ix == size = reverse counts
            | otherwise =
                let c = (head counts) + (M.findWithDefault 0 ix started) - (M.findWithDefault 0 (ix - 1) finished)
                in go (c : counts) (ix + 1)


calcOccurrences :: [Interval] -> Int -> [(Int, Int)]
calcOccurrences intervals n =
    let smallest = minimum $ map left intervals
        largest = maximum $ map right intervals
        size = largest - smallest + 1
        (started, finished) = calcIntervalEndpointCounts intervals smallest
        counts = calcCounts started finished size
    in map (\(count, ix) -> (ix + smallest, count)) $ filter ((/= 0) . fst) $ zip counts [0..]


main :: IO()
main = do
    n <- readLn
    intervals <- readIntervals n
    let result = calcOccurrences intervals n
    mapM_ (\(i, j) -> putStrLn $ unwords $ map show [i, j]) result
