module MathSolver.Types where

data QuestionType = Quantity { subject :: Name }    -- How many does X have?
                  | Gain { subject :: Name }        -- How many has X gained?
                  | Loss { subject :: Name }        -- How many has X lost?
                  | Compare { subject :: Name       -- How many more does X have than Y?
                            , against :: Name }
                  | Combine Name Name               -- How many do X and Y have total?
                  | CombineAll                      -- How many are there total?
--                | Duration                        -- How long did it take?
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
                   , item ::Item
                   , to :: Name }
            | TakeFrom { amount :: Integer  -- Owner takes items from a target
                       , item ::Item
                       , from :: Name }
                deriving (Show, Eq)

type Name = String
type Item = String
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

data Answer = Answer { answerType :: QuestionType
                     , total :: Amount
                     , itemAnswered :: Item }
                 deriving (Show, Eq)
