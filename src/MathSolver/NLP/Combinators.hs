{-# LANGUAGE OverloadedStrings #-}

module MathSolver.NLP.Combinators where

import Prelude hiding (compare)

import Data.Maybe (isJust, fromJust)
import Data.Text (Text)
import qualified Data.Text as T

import Text.Parsec (unexpected, eof)
import qualified Text.Parsec.Combinator as PC
import Text.Parsec.Prim (lookAhead, (<|>), try, token )
import Text.Read (readEither)

import qualified NLP.Corpora.Brown as B
import NLP.Extraction.Parsec (Extractor, posTok, txtTok, anyToken, oneOf, followedBy, posPrefix)
import NLP.Types (POS(..), ChunkOr(..), CaseSensitive(..), toEitherErr)
import NLP.Types.Tags (Tag(..), ChunkTag(..))
import NLP.Types.Tree (mkChunk, Token(..), ChunkOr(..), ChunkedSentence(..), Chunk(..), showTok)

import MathSolver.NLP.WordNum


data C_Owner    = C_Owner   { subjTitle  :: Maybe (POS B.Tag)       -- Owner Title
                            , fromSubj   :: POS B.Tag           }   -- Owner Name
        deriving (Show, Eq)
newtype C_Subj  = C_Subj    { subjToOwner :: C_Owner            }
        deriving (Show, Eq)
data C_Subj2    = C_Subj2   { subj1      :: C_Owner                 -- First subject
                            , subj2      :: C_Owner                 -- Second subject
                            , isBoth     :: Maybe (POS B.Tag)   }   -- Does it include both?
                | C_They    { fromThey   :: POS B.Tag               -- "they" vs "we"
                            , isBoth     :: Maybe (POS B.Tag)   }   -- Does it include both?
        deriving (Show, Eq)
data C_Targ     = C_Targ    { targTitle  :: Maybe (POS B.Tag)       -- Owner Title
                            , fromTarg   :: POS B.Tag           }   -- Owner Name
        deriving (Show, Eq)

newtype C_Qty   = C_Qty     { fromQty    :: [POS B.Tag]         }   -- Quantity
        deriving (Show, Eq)

newtype C_Change = C_Change { changeDir  :: POS B.Tag           }   -- Direction of change
        deriving (Show, Eq)

data C_Obj      = C_Obj     { objAdj1    :: Maybe (POS B.Tag)       -- Primary obj's actective
                            , objItem    :: ObjOrMore               -- Only/Primary Object
                            , objAdj2    :: Maybe (POS B.Tag)       -- Secondary obj
                            , objObj2    :: Maybe (POS B.Tag)   }   -- Secondary obj's adjective
        deriving (Show, Eq)
data ObjOrMore  = Obj { fromObj :: POS B.Tag }
                | More { fromMore :: (POS B.Tag) }
        deriving (Show, Eq)

isObj :: ObjOrMore -> Bool
isObj (Obj _) = True
isObj _ = False

isMore :: ObjOrMore -> Bool
isMore (More _) = True
isMore _ = False


newtype C_Verb  = C_Verb    { fromVerb   :: POS B.Tag           }   -- Verb phrase
        deriving (Show, Eq)

data C_ActP     = C_AP_Set  { actVerb :: C_Verb                     -- Verb to set inventory
                            , actQty  :: C_Qty                      -- Number of items
                            , actObj  :: Maybe C_Obj            }   -- Item or a change

                | C_AP_Chg  { actVerb :: C_Verb                     -- Verb to change inventory
                            , actQty  :: C_Qty                      -- Number of items
                            , chgDir  :: Maybe C_Change             -- Direction of change
                            , actObj  :: Maybe C_Obj            }   -- Item or a change

                | C_AP_Give { actVerb :: C_Verb                     -- Verb to give
                            , actQty  :: C_Qty                      -- Number of items
                            , actObj  :: Maybe C_Obj                -- Item or 
                            , target  :: C_Targ }                   -- Direction of change

                | C_AP_Take { actVerb :: C_Verb                     -- Verb to give
                            , actQty  :: C_Qty                      -- Number of items
                            , actObj  :: Maybe C_Obj                -- Item or 
                            , target  :: C_Targ                 }   -- Direction of change
        deriving (Show, Eq)

data C_EvtP     = C_EvtP    { probSubjCh :: C_Subj                  -- Event Subject's name
                            , fromActCh  :: C_ActP              }   -- Event Action Phrase
        deriving (Show, Eq)

               -- Question asking about a quantity. These won't make assumptions on object ambiguity
data C_Qst      = C_Qst_Qty { qstObj    :: C_Obj                    -- Object asked about
                            , qstSubj   :: Maybe C_Subj             -- Subject of question
                            , qstVerb   :: C_Verb                   -- Verb for the question
                            , qstAdv    :: Maybe (POS B.Tag)    }   -- Adverb for when useful

                -- Question asking about a total quantity. These will scope out based on ambiguity
                | C_Qst_Tot { qstObj    :: C_Obj                    -- Object asked about
                            , qstSubj   :: Maybe C_Subj             -- Subject of question
                            , qstVerb   :: C_Verb                   -- Verb for the question
                            , qstAdv    :: Maybe (POS B.Tag)        -- Adverb for when useful
                            , qstTag    :: POS B.Tag            }   -- Word used for "in total"

                -- Question asking about a total quantity of all subjects.
                | C_Qst_CB  { qstObj    :: C_Obj                    -- Object asked about
                            , qstSubjs  :: C_Subj2                  -- Subjects of question
                            , qstVerb   :: C_Verb                   -- Verb for the question
                            , qstAdv    :: Maybe (POS B.Tag)        -- Adverb for when useful
                            , qstOTag   :: Maybe (POS B.Tag)    }   -- Word used for "in total"

                -- Question asking about a total quantity of all subjects.
                | C_Qst_CA  { qstObj    :: C_Obj                    -- Object asked about
                            , qstVerb   :: C_Verb                   -- Verb for the question
                            , qstAdv    :: Maybe (POS B.Tag)        -- Adverb for when useful
                            , qstTag    :: POS B.Tag            }   -- Word used for "in total"

                | C_Qst_Mod { modQAdv    :: POS B.Tag               -- Adverb asked about; e.g. "long"
                            , modQMod    :: POS B.Tag               -- Mode of question; e.g. "will"
                            , modQSubj   :: POS B.Tag               -- Subject of question
                            , modQVerb   :: POS B.Tag           }   -- Verb in time for the question
        deriving (Show, Eq)
newtype C_Comp  = C_Comp    { fromComp :: POS B.Tag             }   -- Comparison against a target
        deriving (Show, Eq)

{--------------------------------------------------------------------------------------------------}
{---                                         VERB CHUNKS                                        ---}
{--------------------------------------------------------------------------------------------------}
{-    All tags are from the Brown Corpus and are defined in the chatter documentation at          -}
{-  https://hackage.haskell.org/package/chatter-0.9.1.0/docs/NLP-Corpora-Brown.html               -}
{--------------------------------------------------------------------------------------------------}

-- Separate from singleV valid cases would likely be specific to algebraic questions
is_v :: Extractor B.Tag C_Verb
is_v = do
    is <- (try (posTok B.BEZ)   -- is
       <|> try (posTok B.BER)   -- are
       <|> try (posTok B.BEDZ)  -- was
           <|> (posTok B.BED))  -- were
    return (C_Verb is)

-- Useful for knowing the event is about setting inventory
has_v :: Extractor B.Tag C_Verb
has_v = do
    is <- (try (posTok B.HVD)   -- had
       <|> try (posTok B.HVZ)   -- has
           <|> (posTok B.HV))   -- have
    return (C_Verb is)

-- Made special case to handle "been"
hasV :: Extractor B.Tag C_Verb
hasV = do
    has <- has_v
    _   <-  PC.optional (posTok B.BEN)  -- been (optional and stripped; eg "had been walked")
    v   <- (try (posTok B.VBD)          -- verb, past tense
        <|> try (posTok B.VBN)          -- verb, past participle
            <|> (posTok B.VBG))         -- verb, present participle (-ing)
    return (C_Verb v)

isV :: Extractor B.Tag C_Verb
isV = do
    is <- is_v
    v   <- (try (posTok B.VBG)  -- verb, present participle (-ing)
        <|> try (posTok B.VBN)  -- verb, past participle
            <|> (posTok B.HVG)) -- having
    return (C_Verb v)

-- >>> parse isVing "ghci" $ head $ tag tgr "is jumping over."
isVing :: Extractor B.Tag C_Verb
isVing = do
    is <- is_v
    v  <- (try (posTok B.VBG)   -- verb, present participle (-ing)
           <|> (posTok B.HVG))  -- having
    return (C_Verb v)

-- parse singleV "gets" $ head $ tag tgr "walked five miles"
singleV :: Extractor B.Tag C_Verb
singleV = do 
    v <- (try (posTok B.VBD)    -- verb, past tense
      <|> try (posTok B.VBZ)    -- verb, present tense
      <|> try (posTok B.VBN)    -- verb, past participle
           <|> brokenVerbs)     -- Verbs that get mis-tagged
    return (C_Verb v)

-- The tagger will sometimes get confused and break on valid problems. This is usually only a big
-- deal with verbs. For example, "grabs" gets tagged as a plural noun. I'll add them here to be
-- checked as a failsafe as I run across them.
brokenVerbs :: Extractor B.Tag (POS B.Tag)
brokenVerbs = oneOf Sensitive (map Token ["grabs", "books"])

-- Any verb form as a convenience function. Using this doesn't give you any semantic hints
verb :: Extractor B.Tag C_Verb
verb = try isVing <|> try hasV <|> try isV <|> try has_v <|> try is_v <|> singleV

-- Includes "have" or any other present verb except "is". Useful for questions.
presentVerb :: Extractor B.Tag C_Verb
presentVerb = do
    _ <- PC.optionMaybe adverb
    v <- try (posTok B.HV) <|> posTok B.VB  -- "have" or present verb
    return (C_Verb v)

{--------------------------------------------------------------------------------------------------}
{---                                        OWNER CHUNKS                                        ---}
{--------------------------------------------------------------------------------------------------}
-- Finds the name of the target Owner in a Give or TakeFrom event. This includes an optional title.
-- "Mrs." is weird, but everything seems ok.

-- try: parse subjName "repl" $ head $ tag tgr "Susan walked ten miles."
-- The subject always comes immediately before a verb in a word problem. These also include
-- nominal pronouns (he, they, etc).
subjName :: Extractor B.Tag C_Subj
subjName = do
    s <- subj
    lookAhead (try (posPrefix "V") <|> try (posPrefix "H") <|> try (posPrefix "R") <|> brokenVerbs)
    return (C_Subj s)

subj :: Extractor B.Tag C_Owner
subj = do
    t <- PC.optionMaybe (oneOf Sensitive (map Token titles)) <|> PC.optionMaybe (posTok B.NN)
    _ <- PC.optionMaybe (posTok B.Term)
    n <- (try (posTok B.NP)                             -- Singular Proper Noun
      <|> try (posTok B.PPS) <|> try (posTok B.PPSS)    -- Personal Nom Pronoun
           <|> posTok B.PPO)                            -- Pers Acc Pronoun; "Wrong" but still used
    return (C_Owner t n)
    where
        titles = ["Mrs", "Missus", "Ms", "Miz", "Mr", "Mister", "Dr", "Doctor", "Doc"]

subjAndSubj :: Extractor B.Tag C_Subj2
subjAndSubj = do
    s1 <- subj
    _ <- txtTok Sensitive (Token "and")
    s2 <- subj
    b <- PC.optionMaybe (txtTok Sensitive (Token "both"))
    lookAhead (try (posPrefix "V") <|> try (posPrefix "H") <|> posPrefix "R")
    return (C_Subj2 s1 s2 b)

-- These don't include nominal pronouns, but do include accusative ones.
targName :: Extractor B.Tag C_Targ
targName = do
    t <- PC.optionMaybe (oneOf Sensitive (map Token titles)) <|> PC.optionMaybe (posTok B.NN)
    d <- PC.optionMaybe (posTok B.Term)
    n <- try (posTok B.NP) <|> try (posTok B.PPS) <|> try (posTok B.PPSS) <|> try (posTok B.PPO)
          <|> posTok B.PPdollar
    return (C_Targ t n)
    where
        titles = ["Mrs", "Missus", "Ms", "Miz", "Mr", "Mister", "Dr", "Doctor", "Doc"]

{--------------------------------------------------------------------------------------------------}
{---                                       ACTION CHUNKS                                        ---}
{--------------------------------------------------------------------------------------------------}

adverb :: Extractor B.Tag (POS B.Tag)
adverb = try (posTok B.RB) <|> try (posTok B.RP) <|> posTok B.RBR

-- Consumes the remainder of a TaggedSentence
consumeRemainder :: Extractor B.Tag [POS B.Tag]
consumeRemainder = PC.manyTill anyToken (posTok B.Term)

-- Consumes a number. In some cases, problems will simple say "another" or "the", which would imply
-- a quantity of 1, so determinants are also allowed.
-- TODO: allow comma-separated blocks
number :: Extractor B.Tag C_Qty
number = do
    n <- PC.many1 (posTok B.CD)
    return (C_Qty n)


-- Parses an object in an event/question.
-- Grammar : (ADJ) N ("on/in/of/with") (ART) (ADJ) (N)
-- >> parse object "repl" $ head $ tag tgr "green boxes of cereal higher than Tom"
object :: Extractor B.Tag C_Obj
object = do
    _ <- PC.optionMaybe (PC.many1 (try (posTok B.DT)    -- determinant  ex: "another"
                                   <|> (posTok B.AP)))  -- determiner ex: "several", "several more"
    a <- PC.optionMaybe (posTok B.JJ)                   -- adjective (non-comparative/superlative)
    n <- (try (posTok B.NN) <|> try (posTok B.NNS)      -- noun(s)
          <|> posTok B.PPO)                             -- pronoun; eg "them"
    _ <- PC.optionMaybe prepNotTransfer                 -- preposition, restrict "to"/"from"
    _ <- PC.optionMaybe (posTok B.AT)                   -- article
    b <- PC.optionMaybe (posTok B.JJ)
    m <- PC.optionMaybe (posTok B.NN) <|> PC.optionMaybe (posTok B.NNS)
    return (C_Obj a (Obj n) b m)
    where
        prepNotTransfer = oneOf Insensitive (map Token ["on", "in", "of", "with"])

-- Optional object where the question implies it. If the object isn't found, it looks for
-- determiners, such as in cases of "Tom gave Jane five more." This is then resolved
-- heuristically in post-processing.
objOrMore :: Extractor B.Tag C_Obj
objOrMore = try object <|> more

more :: Extractor B.Tag C_Obj
more = do
    m <- oneOf Insensitive (map Token ["more", "fewer", "less"])
    return (C_Obj Nothing (More m) Nothing Nothing)


-- Whether something is now more or fewer. Given synonym/antonym checking,
-- this could be expanded to include JJRs (comparative adjectives).
change :: Extractor B.Tag (POS B.Tag)
change = (try (oneOf Insensitive (map Token ["more", "fewer", "less", "another"]))
      <|> try (posTok B.RBR)    -- Comparative adverbs, e.g. "further", "earlier", etc
          <|>  posTok B.JJR)    -- Comparative adjectives, e.g. "taller"

-- This is useful as its own chunk so it can be targeted directly in a record.
changeCh :: Extractor B.Tag C_Change
changeCh = do
    c <- change
    return (C_Change c)

-- Action phrases that set an owner's inventory. These are usually an initial event.
setAP :: Extractor B.Tag C_ActP
setAP = do
    v <- try hasV <|> try isV <|> try has_v <|> is_v
    _ <- PC.optionMaybe (txtTok Insensitive (Token "another"))
    n <- number
    o <- PC.optionMaybe objOrMore
    _ <- consumeRemainder
    return (C_AP_Set v n o)


-- Parses on events that involve addition and subtraction.
changeAP :: Extractor B.Tag C_ActP
changeAP = do
    v   <- verb
    _   <- PC.optionMaybe adverb
    _   <- PC.optionMaybe (posTok B.DT) -- Determinant, e.g. "another", "those", etc
    n   <- number
    dir <- PC.optionMaybe changeCh      -- "more", "fewer", etc
    o   <- PC.optionMaybe object
    _   <- consumeRemainder
    return (C_AP_Chg v n dir o)


giveAP :: Extractor B.Tag C_ActP
giveAP = try giveItToX <|> giveXIt

-- Because the syntax includes this ordering and "to <target>", we know there's a transfer.
-- This parser will only accept a literal "to" token. Therefore, the verb being used has no
-- grammatically correct way of transferring from the target, so it's irrelevant.
-- >>> parse giveItToX "repl" $ head $ tag tgr "handed five apples to Alex."
giveItToX :: Extractor B.Tag C_ActP
giveItToX = do
    v <- verb
    _ <- PC.optionMaybe adverb
    n <- number                         -- Quantity
    _ <- PC.optionMaybe (txtTok Insensitive (Token "of"))
    o <- PC.optionMaybe objOrMore
    _ <- PC.optionMaybe adverb
    _ <- txtTok Insensitive (Token "to")
    t <- targName
    return (C_AP_Give v n o t)

-- This is a slightly unsafe operation, since there might be a 'taking' verb that doesn't require
-- "from". This can be improved by tagger/chunker training or a semantic database.
giveXIt :: Extractor B.Tag C_ActP
giveXIt = do
    v <- verb
    t <- targName
    _ <- PC.optionMaybe (posTok B.DT) -- Determinant, e.g. "another", "those", etc
    n <- number
    _ <- PC.optionMaybe (txtTok Insensitive (Token "of"))
    o <- PC.optionMaybe objOrMore
    return (C_AP_Give v n o t)

-- Because the syntax includes this ordering and "from <target>", the safety is the same as in
-- 'giveItToX'. However, since there is no easily grammatically correct way of taking from someone
-- in a sentence without "from", there won't be a pattern mirroring 'giveXIt'.
takeAP :: Extractor B.Tag C_ActP
takeAP = do
    v <- verb
    _ <- PC.optionMaybe (posTok B.DT)       -- Determinants, e.g. "the", "another", etc
    _ <- PC.optionMaybe (txtTok Insensitive (Token "of"))
    _ <- PC.optionMaybe (posTok B.DT)
    n <- number                             -- Quantity
    o <- PC.optionMaybe objOrMore
    _ <- PC.optionMaybe adverb
    _ <- txtTok Insensitive (Token "from")
    t <- targName
    return (C_AP_Take v n o t)

eventCh :: Extractor B.Tag C_EvtP
eventCh = do
    s <- subjName
    a <- try setAP <|> try takeAP <|> try giveAP <|> changeAP
    return (C_EvtP s a)


{--------------------------------------------------------------------------------------------------}
{---                                      QUESTION CHUNKS                                       ---}
{--------------------------------------------------------------------------------------------------}

-- Comparing against; e.g. "more than"
compare :: Extractor B.Tag C_Comp
compare = do
    c <- change
    _ <- txtTok Sensitive (Token "than")
    return (C_Comp c)


howMany :: Extractor B.Tag (POS B.Tag)
howMany = do
    _ <- txtTok Insensitive (Token "How")
    oneOf Sensitive [Token "many", Token "much"]

does :: Extractor B.Tag (POS B.Tag)
does = try (posTok B.DOZ) <|> try (posTok B.DO) <|> posTok B.DOD

they :: Extractor B.Tag C_Subj2
they = do
    t <- oneOf Insensitive [Token "they", Token "we"]
    b <- PC.optionMaybe (txtTok Sensitive (Token "both"))
    lookAhead (try (posPrefix "V") <|> try (posPrefix "H") <|> posPrefix "R")
    let s = C_Owner Nothing t
    return (C_They t b)

inTotal :: Extractor B.Tag (POS B.Tag)
inTotal = do
    _ <- followedBy anyToken (oneOf Sensitive [Token "altogether", Token "total", Token "all"])
    anyToken


modQtyQst :: Extractor B.Tag C_Qst
modQtyQst = do
    _ <- txtTok Insensitive (Token "How")
    a <- adverb
    m <- posTok B.MD
    s <- try (posTok B.PPS) <|> try (posTok B.NP)
    v <- try (posTok B.VB) <|> posPrefix "BE"
    return (C_Qst_Mod a m s v)

howManyQst :: Extractor B.Tag C_Qst
howManyQst = do
    _ <- howMany
    o <- object
    _ <- does
    s <- PC.optionMaybe subjName
    v <- presentVerb
    a <- PC.optionMaybe adverb
    return (C_Qst_Qty o s v a)

combineBothQst :: Extractor B.Tag C_Qst
combineBothQst = do
    _ <- howMany
    o <- object
    _ <- does
    s <- try subjAndSubj <|> they
    _ <- PC.optionMaybe adverb
    v <- presentVerb
    a <- PC.optionMaybe adverb
    t <- PC.optionMaybe (try inTotal <|> txtTok Sensitive (Token "now"))
    return (C_Qst_CB o s v a t)

combineAllQst :: Extractor B.Tag C_Qst
combineAllQst = do
    _ <- howMany
    o <- object
    _ <- does
    _ <- they
    _ <- PC.optionMaybe adverb
    v <- presentVerb
    a <- PC.optionMaybe adverb
    t <- inTotal
    return (C_Qst_CA o v a t)

totalQst :: Extractor B.Tag C_Qst
totalQst = do
    _ <- howMany
    o <- object
    _ <- does
    s <- PC.optionMaybe subjName
    _ <- PC.optionMaybe adverb
    v <- presentVerb
    a <- PC.optionMaybe adverb
    t <- inTotal
    return (C_Qst_Tot o s v a t)

questionCh :: Extractor B.Tag C_Qst
questionCh = try modQtyQst <|> try combineAllQst <|> try combineBothQst <|> try totalQst
                <|> howManyQst
