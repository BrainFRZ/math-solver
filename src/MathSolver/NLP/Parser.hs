{-# LANGUAGE OverloadedStrings #-}

module MathSolver.NLP.Parser where

import Data.Either
import qualified Data.Map.Strict as M
import Data.Text (Text, intersperse)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
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
{---                                       Pre-Processing                                       ---}
{--------------------------------------------------------------------------------------------------}

{-
Todo:
    Convert "If <event>, <question>" --> <event> <question>
    Convert separate conjunctive sentences into distinct events
-}

preproc :: [TaggedSentence B.Tag] -> [TaggedSentence B.Tag]
preproc txt = txt


{--------------------------------------------------------------------------------------------------}
{---                                      Post-Processing                                       ---}
{--------------------------------------------------------------------------------------------------}

{-
Todo:
    Replace pronouns with most recent Subject; risky without better, but seems safe enough
-}

postprocQst :: C_Qst -> C_Qst
postprocQst qst = qst

postprocEvs :: [C_EvtP] -> [C_EvtP]
postprocEvs evs = evs

{--------------------------------------------------------------------------------------------------}
{---                                       Problem Parser                                       ---}
{--------------------------------------------------------------------------------------------------}

getQst :: [TaggedSentence B.Tag] -> TaggedSentence B.Tag
getQst = last

getEvs :: [TaggedSentence B.Tag] -> [TaggedSentence B.Tag]
getEvs = init


{--------------------------------------------------------------------------------------------------}
{---                                        CHUNK PARSER                                        ---}
{--------------------------------------------------------------------------------------------------}

fromMaybeTag :: Maybe (POS B.Tag) -> Maybe Text
fromMaybeTag Nothing = Nothing
fromMaybeTag (Just t) = Just (showPOStok t)


getProblem :: C_Qst -> [C_EvtP] -> Problem
getProblem qst evs = Problem (getQuestion qst) (getEvents evs)


getOwner :: C_Owner -> Name
getOwner (C_Owner Nothing name) = Name Nothing (showPOStok name)
getOwner (C_Owner (Just title) name) = Name (Just $ showPOStok title) (showPOStok name)

getTarget :: C_Targ -> Name
getTarget (C_Targ Nothing name) = Name Nothing (showPOStok name)
getTarget (C_Targ (Just title) name) = Name (Just $ showPOStok title) (showPOStok name)


getAmount :: C_Qty -> Amount
getAmount q = wordToNum $ T.unwords $ map showPOStok (fromQty q)

getItem :: C_Obj -> Item
getItem (C_Obj adj1 item adj2 obj)
    | isMore item  = Item (fmt adj1) "more" (fmt adj2) (fmt obj)
    | otherwise    = Item (fmt adj1) (showPOStok $ fromObj item) (fmt adj2) (fmt obj)
    where
        fmt = fromMaybeTag

getItemMaybe :: Maybe C_Obj -> Item
getItemMaybe Nothing  = Something
getItemMaybe (Just i) = getItem i



getQuestion :: C_Qst -> Question
getQuestion q@(C_Qst_Mod _ _ _ _) = Question (getQstType q)
                                             (Item Nothing (showPOStok $modQSubj q) Nothing Nothing)
getQuestion q = Question (getQstType q) (getItem $ qstObj q)


getQstType :: C_Qst -> QuestionType
getQstType (C_Qst_Qty _ Nothing _ _)     = Quantity Someone
getQstType (C_Qst_Qty _ (Just s) _ _)    = Quantity (getOwner $ subjToOwner s)
getQstType (C_Qst_Tot _ Nothing _ _ _)   = Total Someone
getQstType (C_Qst_Tot _ (Just s) _ _ _)  = Total (getOwner $ subjToOwner s)
getQstType (C_Qst_CA _ _ _ _)            = CombineAll
getQstType (C_Qst_Mod _ s _ _)           = Quantity (Name Nothing (showPOStok s))  -- Modal qst type
getQstType (C_Qst_CB _ (C_They _) _ _ _) = CombineAll
getQstType (C_Qst_CB _ s _ _ _)          = Combine (getOwner s1) (getOwner s2)
  where
    s1 = subj1 s
    s2 = subj2 s


getEvents :: [C_EvtP] -> [Event]
getEvents = map evtToEvent

evtToEvent :: C_EvtP -> Event
evtToEvent (C_EvtP s a) = Event subj act
  where
    subj = getOwner $ subjToOwner s
    act = getAction a


-- Determines which Solver action the parsed action should map to. Currently the parsed action has
-- far more information than is used, but this can be utilized for further certainty in
-- determining actions.
getAction :: C_ActP -> Action
getAction (C_AP_Set _ qty obj) = Set (getAmount qty) (getItemMaybe obj)
getAction (C_AP_Give _ qty obj to) = Give (getAmount qty) (getItemMaybe obj) (getTarget to)
getAction (C_AP_Take _ qty obj from) = TakeFrom (getAmount qty) (getItemMaybe obj) (getTarget from)
getAction (C_AP_Chg verb qty _ obj) = getChangeAction verb qty obj

-- Determines whether an action should be mapped to an Add or Remove Solver action. Since it's
-- nearly inpossibly to tell syntantically, I use a keywords list. In the future, I hope to make use
-- of a dictionary or synonym/antonym system so the program can learn and grow further.
getChangeAction :: C_Verb -> C_Qty -> Maybe C_Obj -> Action
getChangeAction v q i = vbAction qty item
  where
    verb = showPOStok $ fromVerb v
    qty = getAmount q
    item = getItemMaybe i
    vbAction = fromMaybe Add (M.lookup verb changeActs)


changeActs :: M.Map Text (Integer -> Item -> Action)
changeActs = M.fromList (addList ++ remList)
  where
    addList :: [(Text,(Integer -> Item -> Action))]
    addList = map (\w -> (w, Add)) ["add","added","bought","buys","catches","caught","finds",
        "found","gets","got","grabs","made","makes","picked","picks","played","plays","read",
        "reads","takes","took"]

    remList :: [(Text,(Integer -> Item -> Action))]
    remList = map (\w -> (w, Remove)) ["blew","blows","dropped","drops","dumped","dumps","eats",
        "flew","flies","leaves","left","loses","lost","put","puts","remove","removes","removed"]










