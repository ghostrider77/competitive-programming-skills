import Data.Char(digitToInt)
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Suit = Spades | Hearts | Diamonds | Clubs deriving Eq
data Card = Card {suit :: Suit, rank :: Int}


rankToInt :: Map Char Int
rankToInt = M.fromList [('A', 14), ('K', 13), ('Q', 12), ('J', 11), ('T', 10)]


readCards :: String -> [Card]
readCards line = map (\[r, s] -> Card (charToSuit s) (rank r)) $ words line
    where
        charToSuit :: Char -> Suit
        charToSuit c
            | c == 'S' = Spades
            | c == 'H' = Hearts
            | c == 'D' = Diamonds
            | otherwise = Clubs
        rank :: Char -> Int
        rank r = case M.lookup r rankToInt of Nothing -> digitToInt r
                                              Just n -> n


isStraightFlush :: [Card] -> Bool
isStraightFlush cards =
    let suits = nub $ map (\(Card suit _) -> suit) cards
        checkRankDifferences :: Bool
        checkRankDifferences =
            let ranks = sort $ map (\(Card _ rank) -> rank) cards
                differences = map (\(b, a) -> b - a) $ zip (tail ranks) ranks
            in differences == [1, 1, 1, 1] || differences == [1, 1, 1, 9]
    in if length suits /=1 then False else checkRankDifferences


main :: IO()
main = do
    cards <- fmap readCards getLine
    let result = isStraightFlush cards
    putStrLn $ if result then "YES" else "NO"
