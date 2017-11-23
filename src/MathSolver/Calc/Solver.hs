module MathSolver.Calc.Solver where

import Data.List
import Data.Maybe

import MathSolver.Types


owners :: State -> [Owner]
owners = map fst

items :: Inventory -> [Item]
items = map fst

{--------------------------------------------------------------------------------------------------}
{---                                        ADJUST STATE                                        ---}
{--------------------------------------------------------------------------------------------------}

setItem :: Item -> Amount -> Inventory -> Inventory
setItem item amount inv
    | amount == 0 && hasItem  = is ++ js
    | amount == 0             = inv
    | amount /= 0 && hasItem  = (item, amount) : is ++ js
    | otherwise               = (item, amount) : inv
    where
        hasItem = item `elem` items inv
        (is, _:js) = break (\(i,_) -> i == item) inv

addItem :: Item -> Amount -> Inventory -> Inventory
addItem item amount inv
    | not hasItem && amount == 0  = inv
    | not hasItem                 = (item, amount) : inv
    | total == 0                  = is ++ js
    | total /= 0                  = (item, total) : is ++ js
    | otherwise                   = inv
    where
        hasItem = item `elem` items inv
        (is, i:js) = break (\(i,_) -> i == item) inv
        total = snd i + amount

set :: Owner -> Item -> Amount -> State -> State
set owner item amount state
    | null item                      = state
    | amount == 0 && not ownerKnown  = (owner, []) : state
    | amount /= 0 && not ownerKnown  = (owner, [(item, amount)]) : state
    | otherwise                      = (owner, setItem item amount inv) : xs ++ ys
    where
        ownerKnown = owner `elem` owners state
        (xs, (_,inv):ys) = break (\(o',_) -> o' == owner) state


add :: Owner -> Item -> Amount -> State -> State
add owner item amount state
    | null item                      = state
    | amount == 0 && not ownerKnown  = (owner, []) : state
    | amount /= 0 && not ownerKnown  = (owner, [(item, amount)]) : state
    | otherwise                      = (owner, addItem item amount inv) : xs ++ ys
    where
        ownerKnown = owner `elem` owners state
        (xs, (_,inv):ys) = break (\(o',_) -> o' == owner) state

remove :: Owner -> Item -> Amount -> State -> State
remove owner item amount = add owner item (-amount)

empty :: Owner -> Item -> State -> State
empty owner item = set owner item 0

reset :: Owner -> State -> State
reset owner state
    | owner `elem` owners state = (owner, []) : filter (\(o,_) -> o /= owner) state
    | otherwise                 = state

give :: Owner -> Item -> Amount -> Owner -> State -> State
give owner item amount target state
    | owner == target  = state
    | otherwise        = remove owner item amount $ add target item amount state

takeFrom :: Owner -> Item -> Amount -> Owner -> State -> State
takeFrom owner item amount target state
    | owner == target  = state
    | otherwise        = add owner item amount $ remove target item amount state

{--------------------------------------------------------------------------------------------------}
{---                                         RUN EVENTS                                         ---}
{--------------------------------------------------------------------------------------------------}

-- Solves a question and gives an answer
solve :: Problem -> Answer
solve (question, events) = ask question $ run events
  where
    ask :: Question -> State -> Answer
    ask (Quantity subject, item) state = (Quantity subject, item, hasAmount subject item state)

    ask (Compare subject target, item) state = (Compare subject target, item, diff)
        where diff = hasAmount subject item state - hasAmount target item state 

    ask (Combine subj1 subj2, item) state = (Combine subj1 subj2, item, total)
        where total = hasAmount subj1 item state + hasAmount subj2 item state

    ask (CombineAll, item) state = (CombineAll, item, total)
        where total = sum [amount | (item,amount) <- concatMap snd state]

    -- If no info given, assume they have 0
    hasAmount :: Owner -> Item -> State -> Amount
    hasAmount name item state = fromMaybe 0 (lookup item $ fromMaybe [] (lookup name state))

-- Runs all events on an initial empty problem state
run :: [Event] -> State
run = foldl' eval []

-- Evaluates a single event on the current problem state
eval :: State -> Event -> State
eval state (owner, Set item amount) = set owner item amount state
eval state (owner, Add item amount) = add owner item amount state
eval state (owner, Remove item amount) = remove owner item amount state
eval state (owner, Empty item) = empty owner item state
eval state (owner, Reset) = reset owner state
eval state (owner, Give item amount target) = give owner item amount target state
eval state (owner, TakeFrom item amount target) = takeFrom owner item amount target state

{--------------------------------------------------------------------------------------------------}
{---                                        TEST STATES                                         ---}
{--------------------------------------------------------------------------------------------------}

state1 = [("Tom", [("apples",5), ("bananas",10)])]
state2 = [("Jane",[("apples",10)]),("Tom",[("apples",5),("bananas",10)])]

-- Tom grabs 10 apples from a tree. He gives 2 to Jane. Jane then takes another 3 from Tom.
events1 = [("Tom", Add "apples" 10), ("Tom", Give "apples" 2 "Jane"),
           ("Jane", TakeFrom "apples" 3 "Tom")]
