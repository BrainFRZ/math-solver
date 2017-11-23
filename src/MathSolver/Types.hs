module MathSolver.Types where

data QuestionType = Quantity | Duration | Compare Owner Owner | Combine Owner Owner | CombineAll
data Action = Set               -- Sets an owner's capacity of an item
            | Add               -- Owner gains some of an item
            | Remove            -- Owner loses some of an item
            | Empty             -- Owner loses all of an item
            | Reset             -- Owner loses everything
            | Give Owner        -- Owner gives to a target
            | TakeFrom Owner    -- Owner takes from a target

type Owner = String
type Item = String
type Amount = Integer

type Inventory = [(Item, Amount)]
type State = [(Owner, Inventory)]

type Event = (Owner, Item, Amount, Action)

type Problem = (Question, [Event])
type Question = (Owner, QuestionType, Item)
type Answer = (Owner, QuestionType, Item, Amount)
