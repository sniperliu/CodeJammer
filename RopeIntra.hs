module Main where

import CodeJammer

import System.IO

main :: IO ()
main = do
    input <- fmap (tail . lines) getContents
    let cases = zipWith Case [1..] $ readCases input
    let results = map codeJammer cases
    mapM_ (putStrLn . show) results
    
readCases :: [String] -> [[String]]
readCases [] = []
readCases (x : xs) = let i = (read x) :: Int
                         (pre, post) = splitAt i xs 
                     in pre : (readCases post)

codeJammer :: Case -> Result
codeJammer (Case i p) = Result i $ solve p

solve :: [String] -> String
solve input = let nums = (map ((map read) . words) input) :: [[Int]]
                  solve' (x:[]) = []
                  solve' (x:xs) = (filter (True ==) $ map (crossCheck x) xs) ++ solve' xs 
              in (show . length) $ solve' nums

crossCheck :: [Int] -> [Int] -> Bool
crossCheck (a1:b1:[]) (a2:b2:[]) = ((a1 - a2) * (b1 - b2)) < 0

