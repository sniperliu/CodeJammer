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
    let cases = zipWith Case [1..] $ samples
    mapM_ (putStrLn . show) $ map (codejammer dicts) cases
 
codejammer :: [String] -> Case -> Result
codejammer dict c = Result (casenum c) (solve dict (problem c))
 
solve :: [String] -> String -> String
solve dict input = let chks = map (fmap (==)) $ parseSample input
                       build :: [[Char -> Bool]] -> String -> Bool
                       build chks xs = and $ fmap or $ map (\(fns, x) -> fns <*> (x:[])) $ zip chks xs
                       test = filter (True ==) $ map (build chks) dict
                   in show $ length test
 
parseSample x = case parse tok "" x of Right r -> r
    where tok = many1 $ (between (char '(') (char ')') (many1 lower)) <|> (liftM (:[]) lower)

