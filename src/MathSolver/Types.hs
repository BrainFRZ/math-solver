module MathSolver.Types where

data QuestionType = Quantity { subject :: Owner }   -- How many does X have?
                  | Compare { subject :: Owner      -- How many more does X have than Y?
                            , against :: Owner }
                  | Combine Owner Owner             -- How many do X and Y have total?
                  | CombineAll                      -- How many are there total?
--                | Duration                        -- How long did it take?
                        deriving (Show)

data Action = Set { item :: Item            -- Sets an owner's capacity of an item
                  , amount :: Integer }
            | Add { item :: Item            -- Owner gains some of an item
                  , amount :: Integer }
            | Remove { item :: Item         -- Owner loses some of an item
                     , amount :: Integer }
            | Empty { item :: Item }        -- Owner loses all of an item
            | Reset                         -- Owner loses everything
            | Give { item ::Item            -- Owner gives to a target
                   , amount :: Integer
                   , to :: Owner }
            | TakeFrom { item ::Item        -- Owner takes items from a target
                       , amount :: Integer
                       , from :: Owner }
                deriving (Show)

type Owner = String
type Item = String
type Amount = Integer

type Inventory = [(Item, Amount)]
type State = [(Owner, Inventory)]

type Event = (Owner, Action)

type Problem = (Question, [Event])
type Question = (QuestionType, Item)
type Answer = (QuestionType, Item, Amount)
