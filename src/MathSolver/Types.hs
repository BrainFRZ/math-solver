{-# LANGUAGE OverloadedStrings #-}

module MathSolver.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import qualified Data.Set as S


data QuestionType = Quantity { subject :: Name }    -- How many does X have?
                  | Total { subject :: Name }       -- How much does X have in total?
                  | Gain { subject :: Name }        -- How many has X gained?
                  | Loss { subject :: Name }        -- How many has X lost?
                  | Compare { subject :: Name       -- How many more does X have than Y?
                            , against :: Name }
                  | Combine Name Name               -- How many do X and Y have total?
                  | CombineAll                      -- How many are there total?
        deriving (Show, Eq)

data Action = Set { amount :: Integer       -- Sets an owner's capacity of an item
                  , item :: Item }
            | Add { amount :: Integer       -- Owner gains some of an item
                  , item :: Item }
            | Remove { amount :: Integer    -- Owner loses some of an item
                     , item :: Item }
            | Empty { item :: Item }        -- Owner loses all of an item
            | Reset                         -- Owner loses everything
            | Give { amount :: Integer      -- Owner gives to a target
                   , item   :: Item
                   , to     :: Name }
            | TakeFrom { amount :: Integer  -- Owner takes items from a target
                       , item   :: Item
                       , from   :: Name }
        deriving (Show, Eq)

data Name = Name { title :: Maybe Text, getName :: Text }
          | Someone     -- Owner was implied and couldn't be resolved
          | They { getName :: Text }
instance Show Name where
    show (Name Nothing n) = T.unpack n
    show (Name t n) = T.unpack (fromJust t) ++ ". " ++ T.unpack n

    show Someone = "someone"

    show (They t) = T.unpack t
instance Eq Name where
    Name (Just t) n == Name (Just t') n'  = t == t' && n == n'
    Name _ n == Name _ n'                 = n == n'

    Someone == Name{}                     = True
    Someone == They{}                     = True

    They{} == Name{}                      = True
data Item = Item { itemAdj  :: Maybe Text       -- an adjective, e.g. "large"
                 , fromItem :: Text             -- the main object noun, e.g. "bag"
                 , itemPrep :: Maybe Text       -- a preposition, e.g. "of"
                 , itemObj  :: Maybe Text }     -- Indirect object, e.g. "cereal"

          | Something                           -- Item was implied and couldn't be resolved
instance Show Item where
    show (Item adj itm prep obj) = mtSpace adj ++ T.unpack itm ++ spaceMT prep ++ spaceMT obj
      where
        spaceMT :: Maybe Text -> [Char]
        spaceMT Nothing = ""
        spaceMT (Just t) = " " ++ T.unpack t

        mtSpace :: Maybe Text -> [Char]
        mtSpace Nothing = ""
        mtSpace (Just t) = T.unpack t ++ " "

    show Something = "something"

instance Eq Item where
    Item (Just a) i _ (Just o) == Item (Just a') i' _ (Just o')
            = [a,o] == [a',o'] && i == i'
    Item _ i _ (Just o) == Item _ i' _ (Just o')    = o == o' && i == i'
    Item (Just a) i _ _ == Item (Just a') i' _ _    = a == a' && i == i'
    Item _ i _ _ == Item _ i' _ _                   = i == i'

    Something == Item _ _ _ _                       = True

type Amount = Integer

type Inventory = [(Item, Amount)]
type State = [Owner]

data Owner = NoOne
           | Owner { name :: Name
                   , inventory :: Inventory }
        deriving (Show, Eq)

data Event = Event { owner :: Name
                   , action :: Action }
        deriving (Show, Eq)

data Problem = Problem { question :: Question
                       , events :: [Event] }
        deriving (Show, Eq)

data Question = Question { questionType :: QuestionType
                         , itemAsked :: Item }
        deriving (Show, Eq)

data Answer = Unsolvable
            | Answer { answerType :: QuestionType
                     , total :: Amount
                     , itemAnswered :: Item }
        deriving (Show, Eq)
