module Main where

import Data.List

import System.IO

data Case = Case { casenum :: Int, dataset:: [String] } deriving (Show)
data Result = Result { num :: Int, result :: String }

instance Show Result where
    show (Result i r) = "Case #" ++ (show i) ++ ": " ++ r

main :: IO ()
main = do
   input <- fmap (tail . lines) getContents 
   let cases = zipWith Case [1..] $ splitCases input
   mapM_ (putStrLn . show . solve) cases

splitCases :: [String] -> [[String]]
splitCases input 
    | post == [] = [pre]
    | otherwise = pre : (splitCases post)
    where (pre, post) = splitAt 3 input 

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum $ zipWith (*) xs ys

solve :: Case -> Result
solve input = let
    index = casenum input
    (l1:l2:l3:_) = dataset input
    xs = sort $ map read (words l2)
    ys = (reverse . sort) $ map read (words l3)
    in Result index (show $ scalarProduct xs ys)

{-
solve :: Case -> Result
solve input = Result index (show $ solve' xsl xsr ysl ysr)
    where index = casenum input 
          (l1:l2:l3:_) = dataset input
          -- This solution (permutations) is low effiency
	  -- xs = permutations $ map read $ words l2
	  -- ys = permutations $ map read $ words l3
	  --xs = sort $ map read (words l2)
	  --ys = (reverse . sort) $ map read (words l3)
          (xsl, xsr) = span (<0) $ sort $ map read (words l2)
          (ysl, ysr) = span (<0) $ sort $ map read (words l3)
          solve' (xl:xsl) xsr ysl ysr@(yr:_) = xl * (last ysr) + (solve' xsl xsr ysl $ init ysr) 
          solve' ([]) xsr@(xr:_) ysl@(yl:_) ysr = solve' ysl ysr [] xsr
          solve' xsl@(xl:_) ([]) ysl@(yl:_) ([]) = (last xsl) * (last ysl) + (solve' (init xsl) [] (init ysl) [])
          solve' ([]) (xr:xsr) ([]) (yr:ysr) | xr == 0 = xr * (last ysr) + (solve' [] xsr [] (yr : (init ysr)))
          solve' ([]) xsr ([]) (yr:ysr) | yr == 0 = solve' [] (yr : ysr) [] xsr
          solve' ([]) (xr:xsr) ([]) (yr:ysr) = xr * yr + (solve' [] xsr [] ysr)
          solve' [] [] [] [] = 0 
-}
