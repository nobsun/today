module Main where

import Control.Applicative
import Data.Char
import Data.List
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Math.NumberTheory.Primes
import System.Environment

main :: IO ()
main = dispatch =<< getProgName

dispatch :: String -> IO ()
dispatch "todayo" = today showOrdinalDate
dispatch "todayp" = today showPrimeDate
dispatch "todayw" = today showWeekDate
dispatch _        = today showGregorian

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
