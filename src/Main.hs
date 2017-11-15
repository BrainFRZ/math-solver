module Main (main) where

import System.IO
import MathSolver.NLP.WordNum (numToWord)


main :: IO ()
main = do
    putStr "Enter an integer: "
    n <- readLn
    putStrLn $ numToWord n
