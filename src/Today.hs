module Today where

import Data.Char
import Data.List
import Data.Time
import Data.Time.Calendar
import Math.NumberTheory.Primes

today :: (Day -> String) -> IO ()
today showDate = putStrLn . showDate . localDay
               =<< (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)

showPrimeDate :: Day -> String
showPrimeDate d = case filter isDigit (showGregorian d) of
    ds -> case factorise (read ds :: Integer) of
        [(_, 1)] -> ds ++ " : 今日は素数だがんばろう"
        fs       -> ds ++ " = " ++ showFactors fs ++ " : 今日は素数じゃないね"

showFactors :: [(Prime Integer, Word)] -> String
showFactors = intercalate " * " . map showFactor

showFactor :: (Prime Integer, Word) -> String
showFactor (n,p) = case p of
    1 -> show (unPrime n)
    _ -> show (unPrime n) ++ "^" ++ show p

countPrimeDays :: Integer -> Int
countPrimeDays year
    = length
    $ filter isPrimeNum
    $ map (readInteger . filter isDigit . showGregorian)
    $ [fromGregorian year 1 1 .. fromGregorian year 12 31]

isPrimeNum :: Integer -> Bool
isPrimeNum n = case factorise n of
    [(_,1)] -> True
    _       -> False 

readInteger :: String -> Integer
readInteger = read