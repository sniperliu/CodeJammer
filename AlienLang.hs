module Main where

import Control.Applicative ((<*>))
import Control.Monad
import Data.List
import qualified Data.Set as S
import System.IO
import Text.ParserCombinators.Parsec

data Case = Case { casenum :: Int, problem :: String } deriving (Show)
data Result = Result { num :: Int, result :: String }

instance Show Result where
    show (Result i r) = "Case #" ++ (show i) ++ ": " ++ r

main :: IO ()
main = do
    input <- fmap lines getContents
    let (numchars:numdics:_) = (map read $ (words . head) input) :: [Int]
    let (dicts, samples) = splitAt numdics $ tail input 
    let wordSet = S.fromList $ concatMap tails dicts
    let cases = zipWith Case [1..] $ samples
    mapM_ (putStrLn . show) $ map (codejammer wordSet) cases

codejammer :: S.Set String -> Case -> Result
codejammer dict c = Result (casenum c) (solve dict (problem c))

solve :: S.Set String -> String -> String
solve dict input = let ts = parseSample input
                       --rs = filter (flip S.member dict) $ solve' ts
                   in show $ length (solve' dict ts)

solve' :: S.Set String -> [String] -> [String]
solve' _ ([]) = [[]]
solve' dict (x:xs) = filter (flip S.member dict) $ ((fmap (:) x) <*> (solve' dict xs))


parseSample x = case parse tok "" x of Right r -> r
    where tok = many1 $ (between (char '(') (char ')') (many1 lower)) <|> (liftM (:[]) lower)
