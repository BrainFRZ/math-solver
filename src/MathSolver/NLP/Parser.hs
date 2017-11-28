{-# LANGUAGE OverloadedStrings #-}

module MathSolver.NLP.Parser where

import Data.Either
import Data.Text (Text, intersperse)
import qualified Data.Text as T
import qualified NLP.Corpora.Brown as B
import NLP.Extraction.Parsec
import NLP.POS
import NLP.Types
import NLP.Types.Tags
import NLP.Types.Tree (ChunkOr(..))
import Text.Parsec.Prim ( (<|>), try, parse)
import qualified Text.Parsec.Combinator as PC
import MathSolver.Types
import MathSolver.NLP.WordNum
import MathSolver.NLP.Combinators

{--------------------------------------------------------------------------------------------------}
{---                                        Sentence Type                                       ---}
{--------------------------------------------------------------------------------------------------}

data SentenceType = Evt | Qst
    deriving (Show, Eq)

tsNull :: Tag t => TaggedSentence t -> Bool
tsNull (TaggedSent []) = True
tsNull _ = False

tsType :: Tag tag => TaggedSentence tag -> SentenceType
tsType s
    | punctuation == "?"  = Qst
    | otherwise           = Evt
    where
        punctuation = (showTok . posToken . last . unTS) s

isEvt :: Tag tag => TaggedSentence tag -> Bool
isEvt s = tsType s == Evt

isQst :: Tag tag => TaggedSentence tag -> Bool
isQst s = tsType s == Qst

{--------------------------------------------------------------------------------------------------}
{---                                       Pre-Processing                                       ---}
{--------------------------------------------------------------------------------------------------}

{-
Todo:
    Convert "If <event>, <question>" --> <event> <question>
    Convert separate conjunctive sentences into distinct events
    Delete adverbs
    Expand contraction
-}

{--------------------------------------------------------------------------------------------------}
{---                                      Post-Processing                                       ---}
{--------------------------------------------------------------------------------------------------}

{-
Todo:
    Replace pronouns with Subject; risky, but seems safe enough
    
-}

{--------------------------------------------------------------------------------------------------}
{---                                       Problem Parser                                       ---}
{--------------------------------------------------------------------------------------------------}

getQst :: Tag tag => [TaggedSentence tag] -> TaggedSentence tag
getQst [] = TaggedSent []
getQst ss = last ss

getEvs :: Tag tag => [TaggedSentence tag] -> [TaggedSentence tag]
getEvs [] = [TaggedSent []]
getEvs ss = init ss


getProblem :: Tag tag => [TaggedSentence tag] -> (TaggedSentence tag, [TaggedSentence tag])
getProblem p = (getQst p, getEvs p)


parseQst :: Tag tag => TaggedSentence tag -> QuestionType
parseQst = undefined

parseEvts :: Tag tag => [TaggedSentence tag] -> [Event]
parseEvts = map parseEvt

parseEvt :: Tag tag => TaggedSentence tag -> Event
parseEvt = undefined

parseEvtOwner :: Tag tag => TaggedSentence tag -> Name
parseEvtOwner s = undefined

parseQstOwner :: Tag tag => TaggedSentence tag -> Name
parseQstOwner s = undefined


{--------------------------------------------------------------------------------------------------}
{---                                        CHUNK PARSER                                        ---}
{--------------------------------------------------------------------------------------------------}



problem = "Lucy has six apples. John gives her two more. How many apples does she have now?"
test = do
    tgr <- brownTagger
    let t = tag tgr problem
    let q = rights [parse questionCh "Parser.hs" $ getQst t]
    let es = rights $ map (parse eventCh "ghci") (getEvs t)
    putStr "Question: "
    mapM_ print q
    putStrLn "Events:"
    mapM_ print es
    return ()
