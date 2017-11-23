module MathSolver.Calc.Solver where

import Data.List

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
takeFrom owner item amount target = give target item amount owner

{--------------------------------------------------------------------------------------------------}
{---                                        TEST STATES                                         ---}
{--------------------------------------------------------------------------------------------------}

state1 = [("Tom", [("apples",5), ("bananas",10)])]
state2 = [("Jill",[("apples",10)]),("Tom",[("apples",5),("bananas",10)])]