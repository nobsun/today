module Main where

import System.IO
import System.Environment
import Today

main :: IO ()
main = do
    args <- getArgs
    case args of
        []  -> hPutStrLn stderr "Usage: pdays <year>"
        a:_ -> print $ primeDays (read a)
