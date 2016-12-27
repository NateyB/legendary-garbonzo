import           MDPs
import           Russell
{-
TODO:
Rework utility evaluation
Rework the example MDPs to meet the constraints placed by their redefinition
Rework policy evaluation
-}


-- russellSamplePolicy :: NDimensionalGrid RussellAction
-- russellSamplePolicy = NDimensionalGrid [
--         OneDimensionalGrid [North, East, East, North],
--         OneDimensionalGrid [North, West, West, North],
--         OneDimensionalGrid [North, West, West, South]
--         ]

--------------------------------------------------------------------------------------------------------
-------------------------------------- MDP Solving Utilities -------------------------------------------
--------------------------------------------------------------------------------------------------------
-- Given an MDP with coordinates, calculates the expected utility of the action provided.
expectedUtility :: MDP s action -> [Coord] -> NDimensionalGrid Double -> action -> Double
expectedUtility MDP{state = state, transition = transition} place utilities act = sum $ zipWith (*) utils probs
    where
        utils = fmap (utilities !#!) coords
        (coords, probs) = unzip $ transition state act place

-- Old expected utility (kept around in case I messed up)
-- Given an MDP with coordinates, calculates the expected utility of the action provided.
-- expectedUtility :: MDP a action -> [Int] -> NDimensionalGrid Double -> action -> Double
-- expectedUtility MDP{state=state, getUtilities=getUtils} coords utilities act = sum (zipWith (*) (trans state act coords) (getUtils state coords utilities))

singleUtilityUpdate :: MDP a action -> NDimensionalGrid Double -> Double -> [Coord] -> Double
singleUtilityUpdate mdp@MDP{state = state, reward = reward, actions = actions} utils discount coords = stateReward + discount*futureReward
    where
        stateReward = reward $ state !#! coords
        futureReward = maximum $ fmap (expectedUtility mdp coords utils) moves
        moves = actions state coords

-- Old utility update for a single square
-- Updates the utility for a single tile in the value iteration algorithm
-- singleUtilityUpdate :: MDP a action -> [Int] -> NDimensionalGrid Double -> Double -> Double
-- singleUtilityUpdate mdp@(MDP state acts trans rews acc getUtils) coords utilities discount = rews (acc state coords) + discount * maximum (fmap (expectedUtility mdp coords utilities) acts)

-- -- Updates the utility for a single tile in the policy iteration algorithm
-- singlePolicyUpdate :: MDP a action -> [Int] -> [[[Double]]] -> [[[action]]] -> Double -> Double
-- singlePolicyUpdate mdp@(MDP state acts trans rews acc getUtils) coords utilities policy discount = rews acc state coords + discount * expectedUtility mdp coords utilities policy !! head coords !! (coords !! 1) !! (coords !! 2)

-- Updates the utilities for every tile in the value iteration algorithm
utilityEvaluation :: MDP a action -> NDimensionalGrid Double -> Double -> NDimensionalGrid Double
utilityEvaluation mdp utilities discount = mapCoordinates (singleUtilityUpdate mdp utilities discount . reverse) utilities

-- Old utility update for every tile in the value iteration algorithm
-- Updates the utilities for every tile in the value iteration algorithm
-- utilityEvaluation :: MDP a action -> [[[Double]]] -> Double -> [[[Double]]]
-- utilityEvaluation mdp@(MDP state acts trans rews acc getUtils) utilities discount = mapToAllWithRowColZ (\z col row -> singleUtilityUpdate mdp [z, row, col] utilities discount) utilities

-- -- Updates the utilities for every tile in the policy iteration algorithm
-- policyEvaluation :: MDP a action -> [[[Double]]] -> [[[action]]] -> Double -> [[[Double]]]
-- -- (map (\q -> zipWith (+) [0..((length q) - 1)] q) (map (\x -> replicate (length (q !! x)) x) [0..((length q - 1))]))
-- policyEvaluation mdp@(MDP state acts trans rews acc getUtils) utilities policy discount = mapToAllWithRowColZ (\z col row -> singlePolicyUpdate mdp [z, row, col] utilities policy discount) (replicate (length utilities) (replicate (length $ head utilities) [0..((length . head $ head utilities) - 1)]))
--

-- Given an MDP, utilities, and discount, this function constructs the next iteration of the same
utilityIteration :: (MDP a action, NDimensionalGrid Double, Double) -> (MDP a action, NDimensionalGrid Double, Double)
utilityIteration (mdp, utilities, discount) = (mdp, utilityEvaluation mdp utilities discount, discount)

-- -- Given an MDP, utilities, policy, and discount, this function constructs the next iteration of the same
-- policyIteration :: (MDP a action, [[[Double]]], [[[action]]], Double) -> (MDP a action, [[[Double]]], [[[action]]], Double)
-- policyIteration (mdp@(MDP state acts trans rews acc getUtils), utilities, policy, discount) = (mdp, updatedUtilities, generatePolicy mdp updatedUtilities discount, discount)
--                                                     where updatedUtilities = policyEvaluation mdp utilities policy discount

-- -- Gets the converged policy
-- finishPolicyCheck :: Eq action => [(MDP a action, [[[Double]]], [[[action]]], Double)] -> Int -> Int -> (MDP a action, [[[Double]]], [[[action]]], Double)
-- finishPolicyCheck (x:xs) orig num | num == 0 && samePolicy x (head xs) = x
--                                   | num > 0 && samePolicy x (head xs) = finishPolicyCheck xs orig (num - 1)
--                                   | otherwise = finishPolicyCheck xs orig orig
--                                   where samePolicy (a, b, pol1, c) (d, e, pol2, f) = pol1 == pol2

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

-- Generates a policy given utilities
generatePolicy :: MDP a action -> NDimensionalGrid Double -> NDimensionalGrid action
generatePolicy mdp@MDP{state = state, actions = actions} utilities = mapCoordinates ((\coords -> argmax (expectedUtility mdp coords utilities) (actions state coords)) . reverse) state


-- -- Old method
-- -- Generates a policy given utilities
-- generatePolicy :: MDP a action -> [[[Double]]] -> Double -> [[[action]]]
-- generatePolicy mdp@(MDP state acts trans rews acc getUtils) utilities discount = mapToAllWithRowColZ (\z col row -> (argmax acts (expectedUtility mdp [z, row, col] utilities))) (replicate (length utilities) (replicate (length . head utilities) [0..length . head . head utilities - 1]))

--------------------------------------------------------------------------------------------------------
-------------------------------------------- Main Program ----------------------------------------------
--------------------------------------------------------------------------------------------------------

main = do   putStrLn ""
            -- if finalValPolicy /= finalPolPolicy then do
            --     putStrLn "Policy iteration utilities: "
            --     putStrLn $ showGrid ((++ " | ") . show) finalPolUtils
            -- else
            --     putStr ""
            putStrLn "Value iteration utilities: "
            putStrLn $ showGrid show finalValUtils
            putStrLn "Value final: "
            putStrLn $ showGrid displayAction finalValPolicy
            -- if (finalValPolicy /= finalPolPolicy) then do
                -- putStrLn "Policy final: "
                -- putStrLn $ showGrid displayAction finalPolPolicy
            -- else
            --     putStr ""

                where thisMDP = russell3x4
                    --   thisPolicy = wumpusSamplePolicy
                      theseUtils = fmap (const 0) russellWorld
                    --   polMDPs = iterate policyIteration (thisMDP, theseUtils, generatePolicy thisMDP theseUtils discount, discount)
                      valMDPs = iterate utilityIteration (thisMDP, theseUtils, discount)
                      (finalValMDP, finalValUtils, valDiscount) = finishIterationCheck valMDPs 1e-10
                      finalValPolicy = generatePolicy finalValMDP finalValUtils
                    --   (finalPolMDP, finalPolUtils, finalPolPolicy, polDiscount) = finishPolicyCheck polMDPs 5000 5000
                      discount = 1
