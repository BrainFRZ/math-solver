{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module MathSolver.NLP.Combinators where

import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

import Data.Maybe (isJust, fromJust)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Text.Parsec.Combinator as PC
import Text.Parsec.Prim (lookAhead, (<|>), try)
import Text.Read (readEither)

import qualified NLP.Corpora.Brown as B
import NLP.Extraction.Parsec (Extractor, posTok, txtTok, anyToken, oneOf, followedBy)
import NLP.Types (POS(..), ChunkOr(..), CaseSensitive(..), toEitherErr)
import NLP.Types.Tags (Tag(..), ChunkTag(..))
import NLP.Types.Tree (mkChunk, Token(..), ChunkOr(..), ChunkedSentence(..), Chunk(..), showTok)

import MathSolver.NLP.WordNum


data ProbChunk = C_Prob     -- Problem
               | C_Ev       -- Event
               | C_Qst      -- Question
               | C_Set_AP   -- Action phrase
               | C_Add_AP   -- Add/Get Action phrase
               | C_Rem_AP   -- Rem/Drop Action phrase
               | C_Give_AP  -- GiveTo Action phrase
               | C_Take_AP  -- TakeFrom Action phrase
               | C_VP       -- Verb phrase
               | C_Owner    -- Owner name
               | C_Trans    -- Transfer chunk
               | C_Qty      -- Quantity
               | C_Change   -- Direction of change
               | C_Comp     -- Comparison against a target
               | C_Obj      -- Object
               | C_O        -- "Out" not a chunk
    deriving (Read, Show, Eq, Ord, Enum, Generic, Bounded)

instance ChunkTag ProbChunk where
    fromChunk = T.pack . show
    parseChunk txt = toEitherErr $ readEither (T.unpack $ T.append "C_" txt)
    notChunk = C_O

instance Serialize ProbChunk

instance Arbitrary ProbChunk where
    arbitrary = elements [minBound ..]

extractChunks :: Either a (ChunkOr ProbChunk tag) -> (ProbChunk, Text)
extractChunks (Left _) = (notChunk, T.empty)
extractChunks (Right (Chunk_CN (Chunk chunk tag))) = (chunk, T.unwords $ map extractPOS tag)
extractPOS (POS_CN p) = (showTok . posToken) p

{--------------------------------------------------------------------------------------------------}
{---                                         VERB CHUNKS                                        ---}
{--------------------------------------------------------------------------------------------------}
{-    All tags are from the Brown Corpus and are defined in the chatter documentation at          -}
{-  https://hackage.haskell.org/package/chatter-0.9.1.0/docs/NLP-Corpora-Brown.html               -}
{--------------------------------------------------------------------------------------------------}

-- Made special case to handle "been"
hadV :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
hadV = do
    had <- (try (posTok B.HVD)          -- had (stripped)
        <|> try (posTok B.HVZ)          -- has
            <|> (posTok B.HV))          -- have (from "they have" being messed up)
    _   <-  PC.optional (posTok B.BEN)  -- been (optional and stripped; eg "had been walked")
    v   <- (try (posTok B.VBD)          -- verb, past tense
        <|> try (posTok B.VBN)          -- verb, past participle
            <|> (posTok B.VBG))         -- verb, present participle (-ing)
    return (mkChunk C_VP [POS_CN v])

wasV :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
wasV = do
    was <- (try (posTok B.BEDZ) -- was (stripped)
            <|> (posTok B.BED)) -- were (stripped)
    v   <- (try (posTok B.VBG)  -- verb, present participle (-ing)
        <|> try (posTok B.VBN)  -- verb, past participle
            <|> (posTok B.HVG)) -- having
    return (mkChunk C_VP [POS_CN v])

-- parse isVing "gets" $ head $ tag tgr "is jumping over."
isVing :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
isVing = do
    is <- (try (posTok B.BEZ)   -- Can't re-use is_v because it'd nest chunks
           <|> (posTok B.BER))  -- are
    v  <- (try (posTok B.VBG)   -- verb, present participle (-ing)
           <|> (posTok B.HVG))  -- having
    return (mkChunk C_VP [POS_CN v])

-- Separate from singleV valid cases would likely be specific to algebraic questions
is_v :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
is_v = do
    is <- (try (posTok B.BEZ)   -- is
       <|> try (posTok B.BER)   -- are
           <|> (posTok B.BEDZ)) -- was ()
    return (mkChunk C_VP [POS_CN is])

-- Useful for knowing the event is about setting inventory
has_v :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
has_v = do
    is <- (try (posTok B.HVD)   -- had
           <|> (posTok B.HVZ))  -- has
    return (mkChunk C_VP [POS_CN is])

-- parse singleV "gets" $ head $ tag tgr "walked five miles"
singleV :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
singleV = do 
    v <- (try (posTok B.VBD)    -- verb, past tense
      <|> try (posTok B.VBZ)    -- verb, present tense
          <|> (posTok B.VBN))   -- verb, past participle
    return (mkChunk C_VP [POS_CN v])

-- Any verb form as a convenience function. Using this doesn't give you any semantic hints
verb :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
verb = try is_v <|> try has_v <|> singleV <|> try hadV <|> try wasV <|> try isVing

{--------------------------------------------------------------------------------------------------}
{---                                        OWNER CHUNKS                                        ---}
{--------------------------------------------------------------------------------------------------}

nameToken :: Extractor B.Tag (POS B.Tag)
nameToken = posTok B.NP

-- parse evtSubjName "World" $ head $ tag tgr "On Sunday, Susan walked ten miles."
-- The subject always comes immediately before a verb in a word problem.
-- We can also skip initial irrelevant info in any problem.
evtSubjName :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
evtSubjName = do 
    _ <- PC.manyTill anyToken (lookAhead nameToken)
    n <- nameToken
    lookAhead verb
    return (mkChunk C_Owner [POS_CN n])

-- Finds the name of the target Owner in a Give or TakeFrom event
evtOwner :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
evtOwner = do
    n <- nameToken -- Singular Proper Noun
    return (mkChunk C_Owner [POS_CN n])

{--------------------------------------------------------------------------------------------------}
{---                                       ACTION CHUNKS                                        ---}
{--------------------------------------------------------------------------------------------------}

number :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
number = do
    n <- PC.many1 $ numWord `PC.sepBy1` (txtTok Sensitive ",")
    return $ (mkChunk C_Qty . map POS_CN . concat) n
    where
      numWord = posTok B.CD

-- parse object "ghci" $ head $ tag tgr "green boxes of cereal higher than Tom"
object :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
object = do
    a <- PC.optionMaybe (posTok B.JJ)     -- adjective (non-comparative/superlative)
    n <- try (posTok B.NN) <|> posTok B.NNS
    _ <- PC.optionMaybe (posTok B.IN)
    m <- PC.optionMaybe (posTok B.NN) <|> PC.optionMaybe (posTok B.NNS)
    return (mkChunk C_Obj $ map (POS_CN . fromJust) (filter isJust [a, Just n, m]))

-- Whether something is now more or fewer. Given synonym/antonym checking,
-- this could be expanded to include JJRs (comparative adjectives).
change :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
change = do
    c <- oneOf Insensitive (map Token ["more", "fewer", "less"])
    return (mkChunk C_Change [POS_CN c])


{--------------------------------------------------------------------------------------------------}
{---                                      QUESTION CHUNKS                                       ---}
{--------------------------------------------------------------------------------------------------}

-- Comparing against; e.g. "more than"
compare :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
compare = do
    c <- oneOf Insensitive (map Token ["more", "fewer", "less"])
    t <- txtTok Insensitive (Token "than")
    return (mkChunk C_Comp [POS_CN c, POS_CN t])
