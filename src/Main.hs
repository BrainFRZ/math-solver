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

menuOpts :: M.Map Text (POSTagger B.Tag -> IO ())
menuOpts = M.fromList [("WFI", wordFromInt), ("TP", tagProblem), ("PP", parseProblem)]

main :: IO ()
main = do 
    putStrLn "Loading Brown's Corpus..."
    tagger <- brownTagger
    menu tagger

menu :: POSTagger B.Tag -> IO ()
menu tgr = do
    putStrLn $ T.unwords $ M.keys menuOpts
    putStr "Enter an option: "
    opt <- getLine
    case M.lookup opt menuOpts of
        Just o  ->  execute o tgr >> menu tgr
        Nothing ->  if opt `elem` ["quit", "exit"]
                        then return ()
                        else putStrLn "Not an option" >> menu tgr

execute :: (POSTagger B.Tag -> IO ()) -> POSTagger B.Tag -> IO ()
execute fun = fun


wordFromInt :: POSTagger B.Tag -> IO ()
wordFromInt tgr = do putStr "Enter an integer: "
                     line <- getLine
                     case readMaybe (T.unpack line) of
                        Just n  -> putStrLn $ numToWord n
                        Nothing -> putStrLn "Not an integer" >> wordFromInt tgr

tagProblem :: POSTagger B.Tag -> IO ()
tagProblem tgr = do putStrLn "Loading Brown tagger ..."
                    putStr "Enter a problem: "
                    line <- getLine
                    print ([line] >>= tag tgr)

parseProblem :: POSTagger B.Tag -> IO ()
parseProblem tgr = do
    putStr "Enter a problem: "
    problem <- getLine
    let p = tag tgr problem
    putStr "Question: "
    mapM_ print $ rights [parse questionCh "Main.hs, line 64" (last p)]
    putStrLn "Events:"
    mapM_ print $ rights (map (parse eventCh "Main.hs, line 66") (init p))
    return ()
