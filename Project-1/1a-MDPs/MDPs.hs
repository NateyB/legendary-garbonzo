{-|
 Module: MDPs
 Description: A few utilities for defining and solving Markov Decision Problems

 This module supplies a couple of out-of-box MDP solving utilities (that do
 require stationarity). Specifically, defining an MDP using the provided
 constructors, you can generate a (policy, utility) tuple from either of the
 defined methods.
-}
module MDPs (NDimensionalGrid(OneDimensionalGrid, NDimensionalGrid),
             Coord, STP, (!#!), MDP(MDP, state, actions, transition, reward),
             showGrid, performValueIteration, performPolicyIteration)  where

-- |A recursive data structure for dealing with arbitrary dimensions.
data NDimensionalGrid object = OneDimensionalGrid [object]
                             | NDimensionalGrid [NDimensionalGrid object]
                             deriving (Show, Eq)

-- |Applying fmap to an n-dimensional grid applies the function to every
-- element in the grid
instance Functor NDimensionalGrid where
    fmap f (OneDimensionalGrid items) = OneDimensionalGrid (fmap f items)
    fmap f (NDimensionalGrid items)   = NDimensionalGrid (fmap (fmap f) items)

-- |Coordinate—used in this module for specifying locations in the nD grid.
type Coord = Int

-- |State transition probability tuple: (New state, transition probability)
type STP = ([Coord], Double)

-- |An accessor for the n-dimensional grid that takes in a list of coordinates
-- and outputs the element at the specified location.
(!#!) :: NDimensionalGrid a -> [Coord] -> a
(OneDimensionalGrid items) !#! [i] = items !! i
(NDimensionalGrid items) !#! (i:is) = (items !! i) !#! is
(OneDimensionalGrid items) !#! b = error $ concat ["Attempt to pass ",
    show (length b), " indices to a single-dimensional grid."]
(NDimensionalGrid items) !#! b = error $ concat ["Attempt to pass ",
    show (length b), " indices to an n-dimensional grid."]

{-|
  MDP is a set of states, actions in those states (here actions are static),
  a transition model, and a reward model.

  Note that this MDP solving utility assumes (and, in fact, requires)
  stationarity in order to reason about state utilities and policies.
-}
data MDP stateComponent action = MDP
    {state     :: NDimensionalGrid stateComponent,
    actions    :: NDimensionalGrid stateComponent -> [Coord] -> [action],
    transition :: NDimensionalGrid stateComponent -> action -> [Coord] -> [STP],
    reward     :: stateComponent -> Double}

--------------------------------------------------------------------------------
------------------------------- Helper utilities -------------------------------
--------------------------------------------------------------------------------
-- |Outputs a formatted nD grid, where elements are converted using the
-- specified function
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

-- |This function maps another function that takes coordinates in reverse
-- order of the grid--that is, least significant coordinate to most
-- signficant. In a 2D grid this corresponds to [column, row] order.
mapCoordinates :: ([Coord] -> b) -> NDimensionalGrid a -> NDimensionalGrid b
mapCoordinates = specialMap []
    where
        specialMap :: [Coord]
            -> ([Coord] -> b)
            -> NDimensionalGrid a
            -> NDimensionalGrid b
        specialMap coords f (OneDimensionalGrid items)
            = OneDimensionalGrid $ fmap (f . (: coords)) [0..length items - 1]
        specialMap coords f (NDimensionalGrid items)
            = NDimensionalGrid $ zipWith
            (\coord item -> specialMap (coord : coords) f item)
            [0..length items - 1] items

--------------------------------------------------------------------------------
----------------------- Generally relevant calculations ------------------------
--------------------------------------------------------------------------------
-- |Generates a policy from an MDP and an nD grid of utilities.
generatePolicy ::
    MDP s action
    -> NDimensionalGrid Double
    -> NDimensionalGrid action
generatePolicy mdp@MDP{state = state, actions = actions} utilities
    = mapCoordinates ((\coords -> argmax
        (expectedUtility mdp coords utilities)
        (actions state coords))
        . reverse) state

-- |Given an MDP with coordinates, a grid of utilities, and an action,
-- calculates the expected utility of the action provided by calculating
-- the sum of the products of the utilities of the new states and the
-- probabilities of reaching those states.
expectedUtility ::
    MDP s action
    -> [Coord]
    -> NDimensionalGrid Double
    -> action
    -> Double
expectedUtility MDP{state = state, transition = transition} place utilities act
    = sum $ zipWith (*) utils probs
    where
        utils = fmap (utilities !#!) coords
        (coords, probs) = unzip $ transition state act place

--------------------------------------------------------------------------------
------------------------------ Value Iteration ---------------------------------
--------------------------------------------------------------------------------
-- |Given an MDP, utilities, discount, and a state, updates the utility of that
-- state according to the reward & expected utility of future actions
singleUtilityUpdate ::
    MDP a action
    -> NDimensionalGrid Double
    -> Double
    -> [Coord]
    -> Double
singleUtilityUpdate mdp@MDP{state = state, reward = reward, actions = actions}
    utils discount coords = stateReward + discount*futureReward
    where
        stateReward = reward $ state !#! coords
        futureReward = maximum $ fmap (expectedUtility mdp coords utils) moves
        moves = actions state coords

-- |Updates the utilities for every tile in the value iteration algorithm
-- according to the process describes in the singleUtilityUpdate function
utilityEvaluation ::
    MDP a action
    -> NDimensionalGrid Double
    -> Double
    -> NDimensionalGrid Double
utilityEvaluation mdp utilities discount =
        mapCoordinates (singleUtilityUpdate mdp utilities discount
        . reverse) utilities


-- |Given an MDP, utilities, and discount, this function constructs
-- the next iteration of the same values
valueIteration ::
    (MDP a action, NDimensionalGrid Double, Double)
    -> (MDP a action, NDimensionalGrid Double, Double)
valueIteration (mdp, utilities, discount)
    = (mdp, utilityEvaluation mdp utilities discount, discount)


-- |Gets the final iteration check; determines if the maximum change
-- between any two states is less than or equal to the second argument,
-- epsilon, *(1 - discount)/discount
-- If this maximum change does not exceed this threshold, the function
-- terminates, returning the most recent value.
finishIterationCheck ::
    (MDP a b, NDimensionalGrid Double, Double)
    -> Double
    -> (MDP a b, NDimensionalGrid Double, Double)
finishIterationCheck (mdp, util1, discount) epsilon
    | maxDifference <= epsilon * (1 - discount)/discount = next_step
    | otherwise = finishIterationCheck next_step epsilon
    where
       next_step@(_, util2, _) = valueIteration (mdp, util1, discount)
       maxDifference = getMaximum differences
       differences = mapCoordinates ((\coords -> abs $ (util1 !#! coords)
           - (util2 !#! coords)) . reverse) util1
       getMaximum (OneDimensionalGrid items) = maximum items
       getMaximum (NDimensionalGrid items)   = maximum $ fmap getMaximum items

--------------------------------------------------------------------------------
------------------------------ Policy Iteration --------------------------------
--------------------------------------------------------------------------------
-- |Updates the utility for a single tile in the policy iteration algorithm,
-- according to the expected utility given by the currently prescribed policy
singlePolicyUpdate ::
    MDP state action
    -> [Coord]
    -> NDimensionalGrid Double
    -> NDimensionalGrid action
    -> Double
    -> Double
singlePolicyUpdate mdp@MDP{state = state, reward = reward} coords
    utilities policy discount = stateReward + discount*futureReward
    where
        stateReward = reward $ state !#! coords
        futureReward = expectedUtility mdp coords utilities (policy !#! coords)

-- |Updates the utilities for every tile in the policy iteration algorithm,
-- according to the method described in the singlePolicyUpdate function above
policyEvaluation ::
    MDP a action
    -> NDimensionalGrid Double
    -> NDimensionalGrid action
    -> Double
    -> NDimensionalGrid Double
policyEvaluation mdp utilities policy discount = mapCoordinates
    ((\coords -> singlePolicyUpdate mdp coords utilities policy discount)
    . reverse) utilities

-- |Given an MDP, utilities, policy, and discount, this function
-- constructs the next iteration of the same values
policyIteration ::
    (MDP a action, NDimensionalGrid Double, NDimensionalGrid action, Double)
    -> (MDP a action, NDimensionalGrid Double, NDimensionalGrid action, Double)
policyIteration (mdp, utilities, policy, discount)
    = (mdp, updatedUtilities, generatePolicy mdp updatedUtilities, discount)
        where updatedUtilities = policyEvaluation mdp utilities policy discount

-- |Returns the converged policy, where convergence is defined as a policy
-- that does not change after replication, the second argument, iterations.
-- Note that replication must be greater than 0.
finishPolicyCheck ::
    Eq act =>
    (MDP a act, NDimensionalGrid Double, NDimensionalGrid act, Double)
    -> Int
    -> (MDP a act, NDimensionalGrid Double, NDimensionalGrid act, Double)
finishPolicyCheck tuple replication = performWork tuple replication replication
    where
        performWork group orig num
            | num == 1 = next
            | num > 1 && samePolicy group next = performWork next orig (num - 1)
            | otherwise = performWork next orig orig
                where
                    next = policyIteration group
                    samePolicy (_, _, pol1, _) (_, _, pol2, _) = pol1 == pol2

--------------------------------------------------------------------------------
----------------------------- Solving Utilities --------------------------------
--------------------------------------------------------------------------------
-- |Given an MDP, a discount factor, and an epsilon threshold, calculates
-- a policy and a set of final utilities according to the value iteration
-- algorithm (i.e., repeatedly udpating tile utilities according to the
-- best possible action that can be taken in those tiles)
performValueIteration ::
    MDP s a
    -> Double
    -> Double
    -> (NDimensionalGrid a, NDimensionalGrid Double)
performValueIteration mdp@MDP{state = curState} discount epsilon
    = (finalPol, finalUtils)
        where
            startUtils = fmap (const 0) curState
            startSituation = (mdp, startUtils, discount)
            (_, finalUtils, _) = finishIterationCheck startSituation epsilon
            finalPol = generatePolicy mdp finalUtils



-- |Given an MDP, a discount factor, and a replication threshold, calculates
-- a policy and a set of utilities according to the policy iteration algorithm
-- (i.e., repeatedly updating the utilities in the polcy and recalculating
-- the policy according to those utilities). Note that the replication threshold
-- represents the number of unchanged policies before the algorithm
-- has been deemed to converge.
performPolicyIteration ::
    Eq a =>
    MDP s a
    -> Double
    -> Int
    -> (NDimensionalGrid a, NDimensionalGrid Double)
performPolicyIteration mdp@MDP{state = curState, actions = getActions}
    discount replication = (finalPol, finalUtils)
        where
            startUtils = fmap (const 0) curState
            tuple = (mdp, startUtils, generatePolicy mdp startUtils, discount)
            (_, finalUtils, finalPol, _) = finishPolicyCheck tuple replication
