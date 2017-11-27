{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (getLine, putStr, putStrLn)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text.IO
import NLP.POS
import Text.Read
import MathSolver.NLP.WordNum (numToWord)
import MathSolver.NLP.Parser


menu :: M.Map T.Text (IO ())
menu = M.fromList [("WFI", wordFromInput), ("PP", parseProblem)]

main :: IO ()
main = do putStrLn $ T.unwords $ M.keys menu
          putStr "Enter an option: "
          opt <- getLine
          case M.lookup opt menu of
            Just o  -> o
            Nothing -> putStrLn "Not an option" >> main

parseProblem :: IO ()
parseProblem = do putStrLn "Loading Brown tagger ..."
                  tagger <- brownTagger
                  putStr "Enter a problem: "
                  line <- getLine
                  print $ tag tagger line

wordFromInput :: IO ()
wordFromInput = do  putStr "Enter an integer: "
                    line <- getLine
                    case readMaybe (T.unpack line) of
                        Just n  -> putStrLn $ numToWord n
                        Nothing -> putStrLn "Not an integer" >> wordFromInput
