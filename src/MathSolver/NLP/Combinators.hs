{-# LANGUAGE OverloadedStrings #-}

module MathSolver.NLP.Combinators where

import Text.Parsec.Prim (lookAhead, (<|>), try)
import qualified NLP.Corpora.Brown as B
import NLP.Extraction.Parsec (Extractor, posTok)
import NLP.Types (Tag(..), POS(..), ChunkOr(..))
import NLP.Types.Tree (mkChunk)


{--------------------------------------------------------------------------------------------------}
{---                                         VERB CHUNKS                                        ---}
{--------------------------------------------------------------------------------------------------}
{-    All tags are from the Brown Corpus and are defined in the chatter documentation at          -}
{-  https://hackage.haskell.org/package/chatter-0.9.1.0/docs/NLP-Corpora-Brown.html               -}
{--------------------------------------------------------------------------------------------------}

-- Made special case to handle "been"
hadV :: Extractor B.Tag (ChunkOr B.Chunk B.Tag)
hadV = do
    had <- (try (posTok B.HVD)  -- had (stripped)
        <|> try (posTok B.HVZ)  -- has
            <|> (posTok B.HV))  -- have (from "they have" being messed up)
    _   <-  try (posTok B.BEN)  -- been (optional and stripped; eg "had been walked")
    v   <- (try (posTok B.VBD)  -- verb, past tense
        <|> try (posTok B.VBN)  -- verb, past participle
            <|> (posTok B.VBG)  -- verb, present participle (-ing)
    return (mkChunk B.C_VP [POS_CN v])


wasV :: Extractor B.Tag (ChunkOr B.Chunk B.Tag)
wasV = do
    was <- (try (posTok B.BEDZ) -- was (stripped)
            <|> (posTok B.BED)) -- were (stripped)
    v   <- (try (posTok B.VBG)  -- verb, present participle (-ing)
        <|> try (posTok B.VBN)  -- verb, past participle
            <|> (posTok B.HVG)) -- having
    return (mkChunk B.C_VP [POS_CN v])

-- parse isVing "gets" $ head $ tag tgr "is jumping over."
isVing :: Extractor B.Tag (ChunkOr B.Chunk B.Tag)
isVing = do
    is <- (try (posTok B.BEZ)   -- Can't re-use is_v because it'd nest chunks
           <|> (posTok B.BER))  -- are
    v  <- (try (posTok B.VBG)   -- verb, present participle (-ing)
           <|> (posTok B.HVG))  -- having
    return (mkChunk B.C_VP [POS_CN v])

-- Separate from singleV valid cases would likely be specific to algebraic questions
is_v :: Extractor B.Tag (ChunkOr B.Chunk B.Tag)
is_v = do
    is <- (try (posTok B.BEZ)   -- is
       <|> try (posTok B.BER)   -- are
           <|> (posTok B.BEN))  -- was
    return (mkChunk B.C_VP [POS_CN is])

-- Useful for knowing the event is about setting inventory
has_v :: Extractor B.Tag (ChunkOr B.Chunk B.Tag)
has_v = do
    is <- (try (posTok B.HVD)   -- had
           <|> (posTok B.HVZ))  -- has
    return (mkChunk B.C_VP [POS_CN is])

-- parse singleV "gets" $ head $ tag tgr "walked five miles"
singleV :: Extractor B.Tag (ChunkOr B.Chunk B.Tag)
singleV = do 
    v <- (try (posTok B.VBD)    -- verb, past tense
      <|> try (posTok B.VBZ)    -- verb, present tense
          <|> (posTok B.VBN))   -- verb, past participle
    return (mkChunk B.C_VP [POS_CN v])

-- Any verb form as a convenience function. Using this doesn't give you any semantic hints
verb :: Extractor B.Tag (ChunkOr B.Chunk B.Tag)
verb = try hadVed <|> try wasVing <|> try isVing <|> try is_v <|> singleV
