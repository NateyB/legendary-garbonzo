module MDPs where

-- | A recursive data structure for dealing with arbitrary dimensions
data NDimensionalGrid object = OneDimensionalGrid [object]
                             | NDimensionalGrid [NDimensionalGrid object]
                             deriving (Show, Eq)

-- | Coordinate
type Coord = Int

-- | State transition probability
type STP = ([Coord], Double)

instance Functor NDimensionalGrid where
    fmap f (OneDimensionalGrid items) = OneDimensionalGrid (fmap f items)
    fmap f (NDimensionalGrid items)   = NDimensionalGrid (fmap (fmap f) items)

-- An accessor for the n-dimensional grid that takes in a list of coordinates
-- and outputs the element at the specified location.
(!#!) :: NDimensionalGrid a -> [Coord] -> a
(OneDimensionalGrid items) !#! [i] = items !! i
(NDimensionalGrid items) !#! (i:is) = (items !! i) !#! is
(OneDimensionalGrid items) !#! b = error $ "Attempt to pass " ++ show (length b)
    ++ " indices to a single-dimensional grid."
(NDimensionalGrid items) !#! b = error $ "Attempt to pass " ++ show (length b)
    ++ " indices to an n-dimensional grid."

{-
MDP is a set of states, actions in those states (here actions are static),
a transition model, and a reward model. Note that our transition model must
also take in coordinates.

Note that this MDP solving utility assumes (and, in fact, requires)
stationarity in order to reason about state utilities and policies.
-}
data MDP stateComponent action = MDP {state :: NDimensionalGrid stateComponent,
                                      actions :: NDimensionalGrid stateComponent -> [Coord] -> [action],
                                      transition :: NDimensionalGrid stateComponent -> action -> [Coord] -> [STP],
                                      reward :: stateComponent -> Double}

--------------------------------------------------------------------------------
------------------------------- Helper utilities -------------------------------
--------------------------------------------------------------------------------
 -- | Formats grid of information using the function specified
showGrid :: (a -> String) -> NDimensionalGrid a -> String
showGrid f (OneDimensionalGrid items) = ((++ " | ") . f) =<< items
showGrid f (NDimensionalGrid items)   = ((++ "\n") . showGrid f) =<< items

-- | Given a function to compare items, gets the item with the largest value
argmax :: (Ord b) => (a -> b) -> [a] -> a
argmax f (q:qs)
        | null qs = q
        | f q < f (head qs) = argmax f qs
        | otherwise = argmax f (q : tail qs)
argmax _ [] = error "Empty list given to argmax function."

-- | This function maps another function that takes coordinates in reverse
--   order of the grid--that is, least significant coordinate to most
--   signficant. In a 2D grid this corresponds to [column, row] order.
mapCoordinates :: ([Coord] -> b) -> NDimensionalGrid a -> NDimensionalGrid b
mapCoordinates = specialMap []
    where
        specialMap :: [Coord] -> ([Coord] -> b) -> NDimensionalGrid a -> NDimensionalGrid b
        specialMap coords f (OneDimensionalGrid items) = OneDimensionalGrid $ fmap (f . (: coords)) [0..length items - 1]
        specialMap coords f (NDimensionalGrid items) = NDimensionalGrid $ zipWith (\coord item -> specialMap (coord : coords) f item) [0..length items - 1] items

--------------------------------------------------------------------------------
----------------------- Generally relevant calculations ------------------------
--------------------------------------------------------------------------------
-- Generates a policy given utilities
generatePolicy :: MDP s action -> NDimensionalGrid Double -> NDimensionalGrid action
generatePolicy mdp@MDP{state = state, actions = actions} utilities = mapCoordinates ((\coords -> argmax (expectedUtility mdp coords utilities) (actions state coords)) . reverse) state

-- Given an MDP with coordinates, calculates the expected utility of the action provided.
expectedUtility :: MDP s action -> [Coord] -> NDimensionalGrid Double -> action -> Double
expectedUtility MDP{state = state, transition = transition} place utilities act = sum $ zipWith (*) utils probs
    where
        utils = fmap (utilities !#!) coords
        (coords, probs) = unzip $ transition state act place

--------------------------------------------------------------------------------
------------------------------ Value Iteration ---------------------------------
--------------------------------------------------------------------------------
singleUtilityUpdate :: MDP a action -> NDimensionalGrid Double -> Double -> [Coord] -> Double
singleUtilityUpdate mdp@MDP{state = state, reward = reward, actions = actions} utils discount coords = stateReward + discount*futureReward
    where
        stateReward = reward $ state !#! coords
        futureReward = maximum $ fmap (expectedUtility mdp coords utils) moves
        moves = actions state coords

-- Updates the utilities for every tile in the value iteration algorithm
utilityEvaluation :: MDP a action -> NDimensionalGrid Double -> Double -> NDimensionalGrid Double
utilityEvaluation mdp utilities discount = mapCoordinates (singleUtilityUpdate mdp utilities discount . reverse) utilities


-- Given an MDP, utilities, and discount, this function constructs the next iteration of the same
utilityIteration :: (MDP a action, NDimensionalGrid Double, Double) -> (MDP a action, NDimensionalGrid Double, Double)
utilityIteration (mdp, utilities, discount) = (mdp, utilityEvaluation mdp utilities discount, discount)


-- Gets the final iteration check
finishIterationCheck :: [(MDP a action, NDimensionalGrid Double, Double)] -> Double -> (MDP a action, NDimensionalGrid Double, Double)
finishIterationCheck ((_, util1, discount):xs) epsilon
    | maxDifference <= epsilon * (1 - discount)/discount = next_step
    | otherwise = finishIterationCheck xs epsilon
       where
           next_step@(_, util2, _) = head xs
           maxDifference = getMaximum differences
           differences = mapCoordinates ((\coords -> abs $ (util1 !#! coords) - (util2 !#! coords)) . reverse) util1

           getMaximum :: Ord a => NDimensionalGrid a -> a
           getMaximum (OneDimensionalGrid items) = maximum items
           getMaximum (NDimensionalGrid items) = maximum $ fmap getMaximum items

finishIterationCheck [] _ = error "Value iteration could not begin."

--------------------------------------------------------------------------------
------------------------------ Policy Iteration ---------------------------------
--------------------------------------------------------------------------------
-- Updates the utility for a single tile in the policy iteration algorithm
singlePolicyUpdate :: MDP state action -> [Coord] -> NDimensionalGrid Double -> NDimensionalGrid action -> Double -> Double
singlePolicyUpdate mdp@MDP{state = state, reward = reward} coords utilities policy discount = stateReward + discount*futureReward
    where
        stateReward = reward $ state !#! coords
        futureReward = expectedUtility mdp coords utilities (policy !#! coords)

-- Updates the utilities for every tile in the policy iteration algorithm
policyEvaluation :: MDP a action -> NDimensionalGrid Double -> NDimensionalGrid action -> Double -> NDimensionalGrid Double
policyEvaluation mdp utilities policy discount = mapCoordinates ((\coords -> singlePolicyUpdate mdp coords utilities policy discount) . reverse) utilities

-- Given an MDP, utilities, policy, and discount, this function constructs the next iteration of the same
policyIteration :: (MDP a action, NDimensionalGrid Double, NDimensionalGrid action, Double) -> (MDP a action, NDimensionalGrid Double, NDimensionalGrid action, Double)
policyIteration (mdp, utilities, policy, discount) = (mdp, updatedUtilities, generatePolicy mdp updatedUtilities, discount)
                                                    where updatedUtilities = policyEvaluation mdp utilities policy discount

-- Gets the converged policy
finishPolicyCheck :: Eq action => [(MDP a action, NDimensionalGrid Double, NDimensionalGrid action, Double)] -> Int -> Int -> (MDP a action, NDimensionalGrid Double, NDimensionalGrid action, Double)
finishPolicyCheck (x:xs) orig num | num == 0 && samePolicy x (head xs) = x
                                  | num > 0 && samePolicy x (head xs) = finishPolicyCheck xs orig (num - 1)
                                  | otherwise = finishPolicyCheck xs orig orig
                                  where samePolicy (_, _, pol1, _) (_, _, pol2, _) = pol1 == pol2
