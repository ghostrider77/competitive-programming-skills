import Control.Monad (replicateM)

data Point = Point { xcoord :: Int, index :: Int }


readPoints :: Int -> IO [Point]
readPoints n = do
    xs <- replicateM n readLn
    return $ map (\(x, ix) -> Point x ix) $ zip xs [1..]


maxDistances :: [Point] -> [(Int, Int)]
maxDistances points = let first @ (Point _ ix) = head points in go (tail points) [(ix, ix)] first first
    where
        go :: [Point] -> [(Int, Int)] -> Point -> Point -> [(Int, Int)]
        go [] acc _ _ = reverse acc
        go (p : xss) acc left right =
            let left' = if xcoord p < xcoord left then p else left
                right' = if xcoord p > xcoord right then p else right
            in go xss ((index left', index right') : acc) left' right'


main :: IO()
main = do
    n <- readLn
    xs <- readPoints n
    let result = maxDistances xs
    mapM_ (\(i, j) -> putStrLn $ unwords $ map show [i, j]) result
