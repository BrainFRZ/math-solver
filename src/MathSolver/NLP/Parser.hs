{-# LANGUAGE OverloadedStrings #-}

module MathSolver.NLP.Parser (getProblem, preproc, postproc, writeAnswer, getQst, getEvs) where

import Data.Bool (bool)
import qualified Data.Map.Strict as M
import Data.Char (toUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, isNothing, isJust, fromJust)
import qualified NLP.Corpora.Brown as B
import NLP.Types
import NLP.Types.Tree (ChunkOr(..), POS(..), showPOStok)
import NLP.Stemmer (Stemmer(English), stem)
import MathSolver.Types
import MathSolver.Calc.Solver
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

-- Pre-Processing resolves various issues after tagging but before parsing
preproc :: [TaggedSentence B.Tag] -> [TaggedSentence B.Tag]
preproc = id


{--------------------------------------------------------------------------------------------------}
{---                                      Post-Processing                                       ---}
{--------------------------------------------------------------------------------------------------}

-- Post-processing resolves various issues after parsing but before solving.
postproc :: (C_Qst, [C_EvtP]) -> (C_Qst, [C_EvtP])
postproc = postprocQst . postprocEvs

postprocQst :: (C_Qst, [C_EvtP]) -> (C_Qst, [C_EvtP])
postprocQst = tryVerbFix . resolveQstPronoun

postprocEvs :: (C_Qst, [C_EvtP]) -> (C_Qst, [C_EvtP])
postprocEvs = resolveImpliedObjs . resolveTargPronouns . resolveEvtPronouns


-- Changes C_He's reference to the last resolved owner in the event list.
resolveQstPronoun :: (C_Qst, [C_EvtP]) -> (C_Qst, [C_EvtP])
resolveQstPronoun (q@C_Qst_Qty{qstSubj = Just C_He{}}, evs)
        = (q{qstSubj = Just (lastSubj evs)}, evs)

resolveQstPronoun (q@C_Qst_Tot{qstSubj = Just C_He{}}, evs)
        = (q{qstSubj = Just (lastSubj evs)}, evs)

resolveQstPronoun prob = prob

lastSubj :: [C_EvtP] -> C_Subj
lastSubj = last . filter isName . owners

isName :: C_Subj -> Bool
isName C_Subj{} = True
isName _        = False

owners :: [C_EvtP] -> [C_Subj]
owners = map probSubjCh

-- Several verb conjugations change from how they are in the question to how they should be in an
-- answer. This function attempts to fix this invisibly by looking for similar verbs with the same
-- subject as the question. If none are found, the verb is left as-is. This won't work for cases
-- where the verb in the question has spelling totally different than any in the event list, or if
-- the verb hasn't been used in the event list. Since the solver works on the question type, the
-- words can be changed without any effect on computation.
tryVerbFix :: (C_Qst, [C_EvtP]) -> (C_Qst, [C_EvtP])
tryVerbFix (q, evs) = (fixVerb q, evs)
  where
    fixVerb :: C_Qst -> C_Qst
    fixVerb qst@C_Qst_Qty{qstVerb = v}  = qst{qstVerb = tryUsedVerbOrHas v}
    fixVerb qst@C_Qst_Tot{qstVerb = v}  = qst{qstVerb = tryUsedVerbOrHas v}
    fixVerb qst = qst

    -- Have and has won't stem to "ha", so they are checked manually because of how common they are
    tryUsedVerbOrHas :: C_Verb -> C_Verb
    tryUsedVerbOrHas v
        | getVerbPos v == B.HV   = C_Verb [POS B.HVZ "has"]
        | otherwise              = grabVerb v evs

    getVerbPos :: C_Verb -> B.Tag
    getVerbPos v = posTag $ last $ fromVerb v

    evtVerb :: C_EvtP -> C_Verb
    evtVerb C_EvtP{fromActCh=act} = actVerb act

    getVerb :: C_Verb -> [Text]
    getVerb v = map showPOStok (fromVerb v)

    grabVerb :: C_Verb -> [C_EvtP] -> C_Verb
    grabVerb C_Verb{fromVerb=[]} _ = error "Parser.grabVerb: Empty verb"
    grabVerb v [] = v
    grabVerb v (e:es) = bool tryLaterVerb thisVerb (isSimilarVerb (getVerb thisVerb) (getVerb v))
      where
        thisVerb     = evtVerb e
        tryLaterVerb = grabVerb v es

        isSimilarVerb :: [Text] -> [Text] -> Bool
        isSimilarVerb v1 v2 = stem English (T.unpack $ last v1) == stem English (T.unpack $ last v2)
--        | stem (getVerb $ evtVerb e) == stem (getVerb v)  = evtVerb e
--        | otherwise                                       = grabVerb v es

subjTxt :: C_Subj -> Text
subjTxt = showPOStok . fromSubj . subjToOwner

evSubj :: C_EvtP -> C_Owner
evSubj = subjToOwner . probSubjCh

-- Pronouns are resolved by using the most recent non-pronoun owner. If the first event uses a
-- pronoun subject, it gets carried
resolveEvtPronouns :: (C_Qst, [C_EvtP]) -> (C_Qst, [C_EvtP])
resolveEvtPronouns (q, evs) = (q, carryLatestOwner (evSubj $ head evs) evs)
  where
    -- Carries the most recent owner over every pronoun subject in an event list
    carryLatestOwner :: C_Owner -> [C_EvtP] -> [C_EvtP]
    carryLatestOwner _ [] = []

    carryLatestOwner subj (e@C_EvtP{probSubjCh=he@C_He{}} : es)
            = e{probSubjCh = he{refOwner = subj}} : carryLatestOwner subj es

    carryLatestOwner _ (e@C_EvtP{probSubjCh=C_Subj{}} : es)
            = e : carryLatestOwner (evSubj e) es

-- Pronouns are resolved by using the most recent non-pronoun owner. If the first event uses a
-- pronoun subject, it gets carried. This should be used after subject pronouns have been resolved
-- to avoid propogating unresolved pronouns.
resolveTargPronouns :: (C_Qst, [C_EvtP]) -> (C_Qst, [C_EvtP])
resolveTargPronouns (q, evs) = (q, targetLatestSubj (evSubj $ head evs) evs)
  where
    -- Carries the most recent owner over every pronoun target in an event list
    targetLatestSubj :: C_Owner -> [C_EvtP] -> [C_EvtP]
    targetLatestSubj _ [] = []

    targetLatestSubj lastSubj (e : es)
        | targImplied (evTarg e)  = applyTarg e : targetLatestSubj (evSubj e) es
        | otherwise               = e : targetLatestSubj (evSubj e) es
        where
            applyTarg :: C_EvtP -> C_EvtP
            applyTarg e@C_EvtP{fromActCh=act@C_AP_Give{}} = e{fromActCh=act{target=(toTarg lastSubj)}}
            applyTarg e@C_EvtP{fromActCh=act@C_AP_Take{}} = e{fromActCh=act{target=(toTarg lastSubj)}}
            applyTarg e = e

            toTarg :: C_Owner -> C_Targ
            toTarg (C_Owner title subj) = C_Targ title subj

    evTarg :: C_EvtP -> Maybe C_Targ
    evTarg C_EvtP{fromActCh=act@C_AP_Give{}} = Just (target act)
    evTarg C_EvtP{fromActCh=act@C_AP_Take{}} = Just (target act)
    evTarg _ = Nothing

    targImplied :: Maybe C_Targ -> Bool
    targImplied (Just t) = (posTag $ fromTarg t) `elem` [B.PPS, B.PPSS, B.PPO, B.PPdollar]
    targImplied Nothing = True

-- Resolves implied objects in events. For example, "John has five books. He gets two more." In this
-- case, the event is implying "more books". The resolver will always use the last explicit object.
resolveImpliedObjs :: (C_Qst, [C_EvtP]) -> (C_Qst, [C_EvtP])
resolveImpliedObjs (q, evs) = (q, carryLatestObj (evObj $ head evs) evs)
  where
    carryLatestObj :: Maybe C_Obj -> [C_EvtP] -> [C_EvtP]
    carryLatestObj _ [] = []

    carryLatestObj obj (e : es)
        | objImplied e  = applyObj e : carryLatestObj obj es
        | otherwise     = e : carryLatestObj (evObj e) es
        where
            applyObj :: C_EvtP -> C_EvtP
            applyObj e@C_EvtP{fromActCh=act@C_AP_Set{}}  = e{fromActCh=act{actObj=obj}}
            applyObj e@C_EvtP{fromActCh=act@C_AP_Chg{}}  = e{fromActCh=act{actObj=obj}}
            applyObj e@C_EvtP{fromActCh=act@C_AP_Give{}} = e{fromActCh=act{actObj=obj}}
            applyObj e@C_EvtP{fromActCh=act@C_AP_Take{}} = e{fromActCh=act{actObj=obj}}

    evObj :: C_EvtP -> Maybe C_Obj
    evObj = actObj . fromActCh

    objImplied :: C_EvtP -> Bool
    objImplied ev = isNothing (evObj ev) || isMore (fromJust $ evObj ev)

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


getProblem :: (C_Qst, [C_EvtP]) -> Problem
getProblem (qst, evs) = Problem (getQuestion qst) (getEvents evs)

getEvents :: [C_EvtP] -> [Event]
getEvents = map evtToEvent

evtToEvent :: C_EvtP -> Event
evtToEvent (C_EvtP he@C_He{} a) = Event (He pronoun ref) (getAction a)
  where
    pronoun :: Text
    pronoun = showPOStok $ fromHe he

    ref :: Name
    ref = getOwner $ refOwner he

evtToEvent (C_EvtP (C_Subj owner) a) = Event (getOwner owner) (getAction a)

getOwner :: C_Owner -> Name
getOwner (C_Owner Nothing name) = Name Nothing (showPOStok name)
getOwner (C_Owner (Just title) name) = Name (Just $ showPOStok title) (showPOStok name)

getTarget :: C_Targ -> Name
getTarget (C_Targ Nothing name) = Name Nothing (showPOStok name)
getTarget (C_Targ (Just title) name) = Name (Just $ showPOStok title) (showPOStok name)


getAmount :: C_Qty -> Amount
getAmount q = wordToNum $ T.unwords $ map showPOStok (fromQty q)

getItem :: C_Obj -> Item
getItem o@(C_Obj adj1 item adj2 obj)
    | isMore o   = Item (fmt adj1) "more" (fmt adj2) (fmt obj)
    | otherwise  = Item (fmt adj1) (showPOStok $ fromObj item) (fmt adj2) (fmt obj)
    where
        fmt = fromMaybeTag

getItemMaybe :: Maybe C_Obj -> Item
getItemMaybe Nothing  = Something
getItemMaybe (Just i) = getItem i



getQuestion :: C_Qst -> Question
getQuestion q@C_Qst_Mod{modQAdv=a, modQMod=m, modQSubj=s, modQVerb=[v]}
    = Question (getQstType q) [showPOStok v] (Item Nothing (showPOStok s) Nothing Nothing)
getQuestion q@C_Qst_QtyIn{qstSubjIn = s, qstVerb = v}
    = Question (getQstType q) (getQstVerb q) (getItem s)
getQuestion q = Question (getQstType q) (getQstVerb q) (getItem $ qstObj q)

getQstVerb :: C_Qst -> [Text]
getQstVerb q = map showPOStok (fromVerb $ qstVerb q)

qstSubjPos :: C_Subj -> B.Tag
qstSubjPos = posTag . fromSubj . subjToOwner

-- Creates an unresolved Name from C_He that will hopefully be resolved in postprocessing
getUnresolvedHeName :: C_Subj -> Name
getUnresolvedHeName C_Subj{} = error "Parser.getUnresolvedHeName: Subject isn't constructed by C_He"
getUnresolvedHeName (C_He name ref) = He heName (getOwner ref)
  where
    heName :: Text
    heName = showPOStok name

-- Determines the question type being asked. If it's asking quantity and the verb is negative, the
-- question is interpreted as a Loss question.
getQstType :: C_Qst -> QuestionType
getQstType C_Qst_Qty{qstSubj = Nothing, qstVerb = v}
    | M.member (getOpVerb v) remList              = Loss Someone
    | otherwise                                   = Quantity Someone

getQstType C_Qst_Qty{qstSubj = Just he@C_He{}, qstVerb = v}
    | M.member (getOpVerb v) remList              = Loss (getUnresolvedHeName he)
    | otherwise                                   = Quantity (getUnresolvedHeName he)

getQstType C_Qst_Qty{qstSubj = Just s, qstVerb = v}
    | M.member (getOpVerb v) remList              = Loss (getOwner $ subjToOwner s)
    | otherwise                                   = Quantity (getOwner $ subjToOwner s)

getQstType C_Qst_QtyIn{qstWhere = Nothing, qstPrep = p} = QuantityIn Someone (getPrep p)
getQstType C_Qst_QtyIn{qstWhere = Just w, qstPrep = p}  = QuantityIn (itemToName $ getItem w) (getPrep p)

getQstType C_Qst_Tot{qstSubj = Nothing}           = Total Someone
getQstType C_Qst_Tot{qstSubj = Just he@C_He{}}    = Total (getUnresolvedHeName he)
getQstType C_Qst_Tot{qstSubj = Just s}            = Total (getOwner $ subjToOwner s)

getQstType C_Qst_CA{}                             = CombineAll

getQstType C_Qst_Mod{modQSubj = s, modQVerb = [v]}      -- Modal qst type
    | M.member (showPOStok v) remList             = Loss (Name Nothing (showPOStok s))
    | otherwise                                   = Quantity (Name Nothing (showPOStok s))

getQstType C_Qst_CB{qstSubjs = (C_They t _), qstVerb = v}
    | M.member (getOpVerb v) remList  = Loss (They (showPOStok t))
    | otherwise                                   = Quantity (They (showPOStok t))

getQstType C_Qst_CB{qstSubjs = s}                 = Combine (getOwner s1) (getOwner s2)
  where
    s1 = subj1 s
    s2 = subj2 s

getPrep :: Maybe (POS B.Tag) -> Text
getPrep p = bool T.empty (showPOStok $ fromJust p) (isJust p)

-- The verb that's doing the operation is always the last verb. For example, in "is reading",
-- "reading" is the verb that needs to be matched, etc.
getOpVerb :: C_Verb -> Text
getOpVerb = showPOStok . last . fromVerb

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
    verb = map showPOStok (fromVerb v)
    qty = getAmount q
    item = getItemMaybe i
    vbAction = bool Add Remove (M.member (getOpVerb v) remList)  -- Defaults to Add


changeActs :: M.Map Text (Integer -> Item -> Action)
changeActs = M.union addList remList

addList :: M.Map Text (Integer -> Item -> Action)
addList = M.fromAscList $ map (\w -> (w, Add)) ["add","added","bought","buy","buys","catch",
    "catches","caught","find","finds","found","get","gets","got","grab","grabs","made","make",
    "makes","pick","picked","picks","play","played","plays","read","reads","take","takes","took"]

remList :: M.Map Text (Integer -> Item -> Action)
remList = M.fromAscList $ map (\w -> (w, Remove)) ["ate", "blew","blow","blows","drop","dropped",
    "drops","dump","dumped","dumps","eat","eats","leave","leaves","left","lose","loses","lost",
    "put","puts","remove","removes","removed"]


{--------------------------------------------------------------------------------------------------}
{---                                       ANSWER PARSER                                        ---}
{--------------------------------------------------------------------------------------------------}

-- Translates the Solver's answer output into plain English
writeAnswer :: Answer -> Text
writeAnswer a = T.pack $ answer $ unwords $ filter (not . null) (showAnswer a)
  where
    answer (c:cs) = toUpper c : cs

showAnswer :: Answer -> [String]
showAnswer Unsolvable = ["There isn't enough information to answer this question. :("]
showAnswer (Answer (Quantity n) vb amt i)
    = [writeName n, writeVerb vb, fromAmt amt, writeItem i ++ "."]
showAnswer (Answer (QuantityIn n prep) vb amt i)
    = [fromAmt amt, writeItem i, writeVerb vb, T.unpack prep, writeItem (nameToItem n) ++ "."]
showAnswer (Answer (Total n) vb amt i)
    = [writeName n, writeVerb vb, fromAmt amt, writeItem i, "in total."]
showAnswer (Answer (Gain n) vb amt i)
    = [writeName n, writeVerb vb, fromAmt amt, "more", writeItem i ++ "."]
showAnswer (Answer (Loss n) vb amt i)
    = [writeName n, writeVerb vb, fromAmt amt, writeItem i ++ "."]
showAnswer (Answer (Compare n targ) vb amt i)
    = [writeName n, writeVerb vb, fromAmt amt, "more", writeItem i, "than", writeName targ ++ "."]
showAnswer (Answer (Combine subj1 subj2) vb amt i)
    = [writeName subj1, "and", writeName subj2, writeVerb vb, fromAmt amt, writeItem i, "combined."]
showAnswer (Answer CombineAll _ amt i)
    = ["There are", fromAmt amt, writeItem i, "altogether."]

-- Converts the solution quantity into a written number
fromAmt :: Amount -> String
fromAmt = T.unpack . numToWord

-- Writes a name from an answer
writeName :: Name -> String
writeName = show

-- Writes an item from an answer
writeItem :: Item -> String
writeItem = show

-- Writes a verb from an answer. This can be vastly improved by reconjugating the verb.
writeVerb :: [Text] -> String
writeVerb = T.unpack . T.unwords
