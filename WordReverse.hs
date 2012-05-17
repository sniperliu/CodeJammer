module Main where

import Data.List

import System.IO

data Case = Case { casenum :: Int, problem :: String } deriving (Show)
data Result = Result { index :: Int, answer :: String } 

instance Show Result where
    show (Result i a) = "Case #" ++ (show i) ++ ": " ++ a

main :: IO ()
main = do
    input <- fmap (tail . lines) getContents
    let cases = zipWith Case [1..] input    
    let results = map solve cases
    mapM_ (putStrLn . show) results

solve :: Case -> Result
solve c = Result { index = casenum c, answer = solve' $ problem c }
    where solve' = unwords . reverse . words
