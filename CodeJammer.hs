module CodeJammer where

data Case = Case { casenum :: Int, problem :: [String] } deriving (Show)
data Result = Result { num :: Int, result :: String }

instance Show Result where
    show (Result i r) = "Case #" ++ (show i) ++ ": " ++ r

