module Main (main) where

import System.IO
import Text.Read
import MathSolver.NLP.WordNum (numToWord)


main :: IO ()
main = do
    putStr "Enter an integer: "
    line <- getLine
    case readMaybe line of
        Just n  -> putStrLn $ numToWord n
        Nothing -> putStrLn "Not an integer" >> main
