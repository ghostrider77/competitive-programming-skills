import Data.Array (Array, listArray, range, (!))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M


convertToIntList :: String -> [Int]
convertToIntList = map read . words


calcCheapestMatrixMultiplication :: Array Int Int -> Int -> Int
calcCheapestMatrixMultiplication sizes n =
    let solveSubproblem :: Map (Int, Int) Int -> (Int, Int) -> Map (Int, Int) Int
        solveSubproblem table (i, j) =
            let values = map(\k -> table M.! (i, k) + table M.! (k+1, j) + sizes ! (i-1) * sizes ! k * sizes ! j)
                             [i..(j-1)]
            in M.insert (i, j) (minimum values) table
        initial = M.fromAscList $ map (\k -> ((k, k), 0)) [1..n]
        dict = foldl solveSubproblem initial [(ix, ix+s) | s <- [1..(n-1)], ix <- [1..(n-s)]]
    in dict M.! (1, n)


main :: IO()
main = do
    nrMatrices <- readLn
    sizes <- fmap (listArray (0, nrMatrices) . convertToIntList) getLine
    let result = calcCheapestMatrixMultiplication sizes nrMatrices
    print result
