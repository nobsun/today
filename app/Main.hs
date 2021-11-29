module Main where

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import System.Environment

import Today

main :: IO ()
main = dispatch =<< getProgName

dispatch :: String -> IO ()
dispatch "todayo" = today showOrdinalDate
dispatch "todayp" = today showPrimeDate
dispatch "todayw" = today showWeekDate
dispatch _        = today showGregorian
