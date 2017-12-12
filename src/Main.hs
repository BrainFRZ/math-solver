{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (getLine, putStr, putStrLn)
import qualified Data.Map as M
import Data.Either (rights)
import Data.Maybe (Maybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (getLine, putStr, putStrLn)
import NLP.POS (tag, brownTagger)
import NLP.Types (POSTagger)
import qualified NLP.Corpora.Brown as B
import Text.Parsec.Prim (parse)
import Text.Read (readMaybe)
import MathSolver.NLP.Combinators
import MathSolver.NLP.Parser
import MathSolver.NLP.WordNum
import MathSolver.Calc.Solver

menuOpts :: M.Map Text (POSTagger B.Tag -> IO ())
menuOpts = M.fromList [("WFI", wordFromInt), ("TP", tagProblem), ("PP", parseProblem),
        ("PPP", procProb), ("DP", displayProblem), ("SP", solveProblem)]

main :: IO ()
main = do
    putStrLn "Loading Brown's Corpus..."
    tagger <- brownTagger
    displayMenu
    menu tagger

displayMenu :: IO ()
displayMenu = do
    putStrLn "Please choose from the following options"
    putStrLn "    WFI: Word from Int"
    putStrLn "    TP:  Tag Problem"
    putStrLn "    PP:  Parse Problem"
    putStrLn "    PPP: Process Problem"
    putStrLn "    DP:  Display Problem"
    putStrLn "    SP:  Solve Problem"

menu :: POSTagger B.Tag -> IO ()
menu tgr = do
    putStr "Enter an option: "
    opt <- getLine
    case M.lookup opt menuOpts of
        Just o  ->  execute o tgr >> menu tgr
        Nothing ->  if opt `elem` ["quit", "exit"]
                        then return ()
                        else putStrLn "Not an option" >> displayMenu >> menu tgr

execute :: (POSTagger B.Tag -> IO ()) -> POSTagger B.Tag -> IO ()
execute fun = fun


solveProblem :: POSTagger B.Tag -> IO ()
solveProblem tgr = do
    putStr "Enter a problem: "
    prob <- getLine
    let p   = preproc $ tag tgr prob
    let q   = head $ rights [parse questionCh "Main.hs, line 62" (getQst p)]
    let evs = rights (map (parse eventCh "Main.hs, line 63") (getEvs p))
    putStrLn $ writeAnswer $ solve (getProblem $ postproc (q, evs))
    menu tgr


wordFromInt :: POSTagger B.Tag -> IO ()
wordFromInt tgr = do putStr "Enter an integer: "
                     line <- getLine
                     case readMaybe (T.unpack line) of
                        Just n  -> putStrLn (numToWord n) >> menu tgr
                        Nothing -> putStrLn "Not an integer" >> wordFromInt tgr

tagProblem :: POSTagger B.Tag -> IO ()
tagProblem tgr = do putStr "Enter a problem: "
                    line <- getLine
                    print ([line] >>= tag tgr)
                    menu tgr

parseProblem :: POSTagger B.Tag -> IO ()
parseProblem tgr = do
    putStr "Enter a problem: "
    problem <- getLine
    let p = tag tgr problem
    putStr "Question: "
    mapM_ print $ rights [parse questionCh "Main.hs, line 87" (getQst p)]
    putStrLn "Events:"
    mapM_ print $ rights (map (parse eventCh "Main.hs, line 89") (getEvs p))

procProb :: POSTagger B.Tag -> IO ()
procProb tgr = do
    putStr "Enter a problem: "
    prob <- getLine
    let p   = preproc $ tag tgr prob
    let q   = head $ rights [parse questionCh "Main.hs, line 96" (getQst p)]
    let evs = rights (map (parse eventCh "Main.hs, line 97") (getEvs p))
    let (pq, pevs) = postproc (q, evs)
    putStr "Question: "
    print pq
    putStrLn "Events:"
    mapM_ print pevs

displayProblem :: POSTagger B.Tag -> IO ()
displayProblem tgr = do
    putStr "Enter a problem: "
    prob <- getLine
    let p   = preproc $ tag tgr prob
    let q   = head $ rights [parse questionCh "Main.hs, line 109" (getQst p)]
    let evs = rights (map (parse eventCh "Main.hs, line 110") (getEvs p))
    putStrLn $ T.pack $ show $ getProblem $ postproc (q, evs)
    menu tgr
