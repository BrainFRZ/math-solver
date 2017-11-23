module MathSolver.Types where

data QuestionType = Quantity | Duration | Compare Owner Owner | Combine Owner Owner | CombineAll
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
                   , target :: Owner }
            | TakeFrom { item ::Item        -- Owner takes items from a target
                       , amount :: Integer
                       , target :: Owner }

type Owner = String
type Item = String
type Amount = Integer

type Inventory = [(Item, Amount)]
type State = [(Owner, Inventory)]

type Event = (Owner, Action)

type Problem = (Question, [Event])
type Question = (Owner, QuestionType, Item)
type Answer = (Owner, QuestionType, Item, Amount)
