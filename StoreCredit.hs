module Main where

import System.IO

data Case = Case { number :: Int, datas :: [String] } deriving (Show)
data Result = Result { casenum :: Int, result :: String }

instance Show Result where
    show (Result i r) = "Case #" ++ (show i) ++ ": " ++ r

main :: IO ()
main = do
    input <- fmap (tail.lines) getContents
    let cases = zipWith Case [1..] $ splitData input
    mapM_ (putStrLn.show) $ map solve cases
     
splitData :: [String] -> [[String]]
splitData input
    | rest == [] = [pre]
    | otherwise = pre : (splitData rest)
    where (pre, rest) = splitAt 3 input

data Item = Item { index :: Int, price :: Int } deriving (Show)

solve :: Case -> Result
solve c = let 
    (l1 : l2 : l3 : _) = datas c
    credit = read l1 :: Int
    items = zipWith Item [1..] $ map read (words l3)
    res = head [(i1 , i2) | i1 <- items, i2 <- items, (index i1) < (index i2), (price i1) + (price i2) == credit] 
    in Result { casenum = number c, result = (show.index.fst $ res) ++ " " ++ (show.index.snd $ res) }
