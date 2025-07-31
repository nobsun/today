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
        [(_, 1)] -> ds ++ " : 今日の日付は素数だね"
        fs       -> ds ++ " = " ++ showFactors fs ++ " : 今日の日付は約数が" ++ show n ++ "個あるね"
            where
                n = foldr (\ (_,x) a -> succ x * a) 1 fs

showFactors :: [(Prime Integer, Word)] -> String
showFactors = intercalate " * " . map showFactor

showFactor :: (Prime Integer, Word) -> String
showFactor (n,p) = case p of
    1 -> show (unPrime n)
    _ -> show (unPrime n) ++ "^" ++ show p

countPrimeDays :: Integer -> Int
countPrimeDays year
    = length $ primeDays year

primeDays :: Integer -> [Integer]
primeDays year = filter isPrimeNum $ yearDays year

yearDays :: Integer -> [Integer]
yearDays year = map (readInteger . filter isDigit . showGregorian)
    [fromGregorian year 1 1 .. fromGregorian year 12 31]

isPrimeNum :: Integer -> Bool
isPrimeNum n = case factorise n of
    [(_,1)] -> True
    _       -> False 

isProdOfTwoPrimes :: Integer -> Bool
isProdOfTwoPrimes n = length (factorise n) == 2

readInteger :: String -> Integer
readInteger = read