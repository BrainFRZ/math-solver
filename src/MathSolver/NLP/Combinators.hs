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
import Text.Parsec.Prim (lookAhead, (<|>), try, token )
import Text.Read (readEither)

import qualified NLP.Corpora.Brown as B
import NLP.Extraction.Parsec (Extractor, posTok, txtTok, anyToken, oneOf, followedBy, posPrefix)
import NLP.Types (POS(..), ChunkOr(..), CaseSensitive(..), toEitherErr)
import NLP.Types.Tags (Tag(..), ChunkTag(..))
import NLP.Types.Tree (mkChunk, Token(..), ChunkOr(..), ChunkedSentence(..), Chunk(..), showTok)

import MathSolver.NLP.WordNum


data ProbChunk = C_Ev       -- Event
               | C_Qst_Qty  -- Question asking about quantity
               | C_Qst_Tot  -- Question asking about a total
               | C_Set_AP   -- Action phrase
               | C_Chg_AP   -- Add/Remove Action phrase
               | C_Give_AP  -- GiveTo Action phrase
               | C_Take_AP  -- TakeFrom Action phrase
               | C_VP       -- Verb phrase
               | C_Subj     -- Owner name
               | C_Targ     -- Target name
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
verb = try singleV <|> try is_v <|> try has_v <|> try hadV <|> try wasV <|> isVing

{--------------------------------------------------------------------------------------------------}
{---                                        OWNER CHUNKS                                        ---}
{--------------------------------------------------------------------------------------------------}

-- Finds the name of the target Owner in a Give or TakeFrom event. This includes an optional title.
-- "Mrs." is weird, but everything seems ok.

-- try: parse subjName "repl" $ head $ tag tgr "Susan walked ten miles."
-- The subject always comes immediately before a verb in a word problem. These also include
-- nominal pronouns (he, they, etc).
subjName :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
subjName = do
    t <- PC.optionMaybe (txtTok Sensitive "Mrs") <|> PC.optionMaybe (posTok B.NP)
    d <- PC.optionMaybe (posTok B.Term)
    n <- try (posTok B.NP) <|> try (posTok B.PPS) <|> posTok B.PPSS -- Singular Proper Noun
    lookAhead (posPrefix "VB")
    return (mkChunk C_Subj $ map (POS_CN . fromJust) (filter isJust [t, d, Just n]))

-- These don't include nominal pronouns, but do include accusative ones.
targName :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
targName = do
    t <- PC.optionMaybe (txtTok Sensitive "Mrs") <|> PC.optionMaybe (posTok B.NP)
    d <- PC.optionMaybe (posTok B.Term)
    n <- try (posTok B.NP) <|> try (posTok B.PPS) <|> try (posTok B.PPSS) <|> posTok B.PPO
    return (mkChunk C_Targ $ map (POS_CN . fromJust) (filter isJust [t, d, Just n]))

{--------------------------------------------------------------------------------------------------}
{---                                       ACTION CHUNKS                                        ---}
{--------------------------------------------------------------------------------------------------}

-- Consumes a number. In some cases, problems will simple say "another" or "the", which would imply
-- a quantity of 1, so determinants are also allowed.
number :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
number = try num <|> another
  where
    num = do
        let numWord = posTok B.CD
        n <- try (PC.many1 $ numWord `PC.sepBy1` (txtTok Sensitive ","))
        return $ (mkChunk C_Qty . map POS_CN . concat) n
    another = do
        n <- posTok B.DT
        return (mkChunk C_Qty [POS_CN n])

-- parse object "repl" $ head $ tag tgr "green boxes of cereal higher than Tom"
object :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
object = do
    a <- PC.optionMaybe (posTok B.JJ)     -- adjective (non-comparative/superlative)
    n <- try (posTok B.NN) <|> try (posTok B.NNS) <|> posTok B.AP
    _ <- PC.optionMaybe prepNotTransfer  -- restrict "to"/"from"
    _ <- PC.optionMaybe (posTok B.AT)
    b <- PC.optionMaybe (posTok B.JJ)
    m <- PC.optionMaybe (posTok B.NN) <|> PC.optionMaybe (posTok B.NNS)
    return (mkChunk C_Obj $ map (POS_CN . fromJust) (filter isJust [a, Just n, b, m]))
    where
        prepNotTransfer = oneOf Insensitive (map Token ["on", "in", "of", "with"])

-- Optional object where the question implies it. Resolving heuristically in post-processing
optObj = PC.option unknown object
  where
    unknown = mkChunk C_Obj [POS_CN $ POS {posTag = B.NN, posToken = Token "unknown"}]

-- Whether something is now more or fewer. Given synonym/antonym checking,
-- this could be expanded to include JJRs (comparative adjectives).
change :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
change = do
    c <- oneOf Insensitive (map Token ["more", "fewer", "less", "another"])
    return (mkChunk C_Change [POS_CN c])


setAP :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
setAP = do
    v <- try hadV <|> try wasV <|> try has_v <|> is_v
    n <- number
    o <- optObj
    return (mkChunk C_Set_AP [v, n, o])


-- Parses on events that involve addition and subtraction.
changeAP :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
changeAP = chg1

chg1 :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
chg1 = do
    v   <- verb
    _   <- PC.optionMaybe (posTok B.RP) -- Adverbs
    _   <- PC.optionMaybe (posTok B.DT) -- Determinant, e.g. "another", "those", etc
    n   <- number
    dir <- PC.optionMaybe change -- "more", "fewer", etc
    o   <- optObj
    return (mkChunk C_Chg_AP $ map fromJust (filter isJust [Just v, Just n, dir, Just o]))

giveAP :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
giveAP = try giveXIt <|> giveItToX

-- Because the syntax includes this ordering and "to <target>", we know there's a transfer.
-- This parser will only accept a literal "to" token. Therefore, the verb being used has no
-- grammatically correct way of transferring from the target, so it's irrelevant.
-- >>> parse giveItToX "repl" $ head $ tag tgr "handed five apples to Alex."
giveItToX :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
giveItToX = do
    let to = txtTok Insensitive (Token "to")
    v <- verb
    n <- number
--    _ <- manyTill anyToken (try object <|> (followedBy anyToken (lookAhead to)))
    o <- object
    _ <- to
    t <- targName
    return (mkChunk C_Give_AP [v, n, o, t])

-- This is a slightly unsafe operation, since there might be a 'taking' verb that doesn't require
-- "from". This can be improved by tagger/chunker training or a semantic database.
giveXIt :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
giveXIt = do
    v <- verb
    t <- targName
    _ <- PC.optionMaybe (posTok B.DT) -- Determinant, e.g. "another", "those", etc
    n <- number
    o <- optObj
    return (mkChunk C_Give_AP [v, n, o, t])

-- Because the syntax includes this ordering and "from <target>", the safety is the same as in
-- 'giveItToX'. However, since there is no easily grammatically correct way of taking from someone
-- in a sentence without "from", there won't be a pattern mirroring 'giveXIt'.
takeAP :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
takeAP = do
    v <- verb
    _ <- PC.optionMaybe (posTok B.DT)
    n <- number
    o <- optObj
    _ <- txtTok Insensitive (Token "from")
    t <- targName
    return (mkChunk C_Take_AP [v, n, o, t])

event :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
event = do
    s <- subjName
    a <- try setAP <|> try giveAP <|> try takeAP <|> changeAP
    _ <- PC.manyTill anyToken (txtTok Sensitive ".")
    return (mkChunk C_Ev [s, a])


{--------------------------------------------------------------------------------------------------}
{---                                      QUESTION CHUNKS                                       ---}
{--------------------------------------------------------------------------------------------------}

-- Comparing against; e.g. "more than"
compare :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
compare = do
    c <- oneOf Insensitive (map Token ["more", "fewer", "less"])
    t <- txtTok Insensitive (Token "than")
    return (mkChunk C_Comp [POS_CN c, POS_CN t])

howMany1 :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
howMany1 = do
    _ <- txtTok Insensitive (Token "How")
    _ <- oneOf Insensitive [Token "many", Token "much"]
    o <- object
    _ <- oneOf Insensitive [Token "did", "does"]
    s <- PC.optionMaybe subjName
    v <- posTok B.VB
    _ <- PC.optionMaybe (txtTok Insensitive "now")
    _ <- txtTok Insensitive (Token "?")
    return (mkChunk C_Qst_Qty $ map fromJust (filter isJust [Just o, s, Just (POS_CN v)]))

total :: Extractor B.Tag (ChunkOr ProbChunk B.Tag)
total = do
    _ <- txtTok Insensitive (Token "How")
    _ <- oneOf Insensitive [Token "many", Token "much"]
    o <- optObj
    _ <- oneOf Insensitive [Token "did", "does"]
    s <- PC.optionMaybe subjName
    v <- posTok B.VB
    _ <- followedBy anyToken (oneOf Insensitive [Token "altogether", Token "total", Token "all"])
    return (mkChunk C_Qst_Tot $ map fromJust (filter isJust [Just o, s, Just (POS_CN v)]))
