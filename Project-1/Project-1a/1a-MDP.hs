import Russell

--------------------------------------------------------------------------------------------------------
--------------------------------- Important Data Structures --------------------------------------------
--------------------------------------------------------------------------------------------------------
data NDimensionalGrid object = OneDimensionalGrid [object]
                             | NDimensionalGrid [NDimensionalGrid object]
                             deriving (Show, Eq)

instance Functor NDimensionalGrid where
    fmap f (OneDimensionalGrid items) = OneDimensionalGrid (fmap f items)
    fmap f (NDimensionalGrid items) = NDimensionalGrid (fmap (fmap f) items)

{-
MDP is a set of states, actions in those states (here actions are static),
a transition model, and a reward. Note that our transition model must
also take in coordinates.
As a final point of note, the MDP must also pass an accessor
so that the coordinates can be used.
-}
data MDP stateComponent action = MDP {state :: NDimensionalGrid stateComponent,
    actions :: NDimensionalGrid stateComponent -> [Int] -> [action],
    transition :: stateComponent -> action -> [Int] -> [Double],
    reward :: stateComponent -> Double,
    accessor :: NDimensionalGrid stateComponent -> [Int] -> stateComponent,
    getUtilities :: NDimensionalGrid stateComponent -> [Int] -> NDimensionalGrid Double -> [Double]}

--------------------------------------------------------------------------------------------------------
------------------------------------------ Helper utilities --------------------------------------------
--------------------------------------------------------------------------------------------------------
 -- Formats a grid of information using the function specified
showGrid :: (a -> String) -> NDimensionalGrid a -> String
showGrid f (OneDimensionalGrid items) = (concatMap) ((++ " ") . f) items
showGrid f (NDimensionalGrid items) = (concatMap ((++ "\n") . (showGrid f)) items) ++ "\n"

argmax :: [a] -> (a -> Double) -> a;
argmax (q:qs) f
        | (null qs) = q
        | ((f q) < (f (head qs))) = argmax qs f
        | otherwise = argmax (q:(tail qs)) f

-- | This function maps another function that takes coordinates in reverse
--   order of the grid--that is, least significant coordinate to most
--   signficant. It maps this function to all relevant parts of the grid.
mapWithCoordinates :: ([Int] -> b) -> NDimensionalGrid a -> NDimensionalGrid b
mapWithCoordinates f grid = specialMap (f) [] grid
    where
        specialMap :: ([Int] -> b) -> [Int] -> NDimensionalGrid a -> NDimensionalGrid b
        specialMap f coords grid@(OneDimensionalGrid items) = OneDimensionalGrid $ fmap (f . (flip (:) coords)) [0..(length items) - 1]
        specialMap f coords grid@(NDimensionalGrid items) = NDimensionalGrid $ zipWith (\item coord -> specialMap (f) (coord : coords) item) items [0..(length items) - 1]

--------------------------------------------------------------------------------------------------------
-------------------------------------- MDP Solving Utilities -------------------------------------------
--------------------------------------------------------------------------------------------------------
-- Given an MDP with coordinates, calculates the expected utility of the action provided.
expectedUtility :: (MDP a action) -> [Int] -> NDimensionalGrid Double -> action -> Double
expectedUtility (MDP{state=state, getUtils=getUtilities}) coords utilities act = sum (zipWith (*) (trans state act coords) (getUtils state coords utilities))

-- Updates the utility for a single tile in the value iteration algorithm
singleUtilityUpdate :: (MDP a action c) -> [Int] -> NDimensionalGrid Double -> Double -> Double
singleUtilityUpdate mdp@(MDP state acts trans rews acc getUtils) coords utilities discount = (rews $ acc state coords) + (discount * (maximum (fmap (expectedUtility mdp coords utilities) acts)))

-- Updates the utility for a single tile in the policy iteration algorithm
singlePolicyUpdate :: (MDP a action c) -> [Int] -> [[[Double]]] -> [[[action]]] -> Double -> Double
singlePolicyUpdate mdp@(MDP state acts trans rews acc getUtils) coords utilities policy discount = (rews $ acc state coords) + (discount * (expectedUtility mdp coords utilities (policy !! (coords !! 0) !! (coords !! 1) !! (coords !! 2))))

-- Updates the utilities for every tile in the value iteration algorithm
utilityEvaluation :: (MDP a action c) -> [[[Double]]] -> Double -> [[[Double]]]
utilityEvaluation mdp@(MDP state acts trans rews acc getUtils) utilities discount = (mapToAllWithRowColZ (\z -> \col -> \row -> singleUtilityUpdate mdp [z, row, col] utilities discount) utilities)

-- Updates the utilities for every tile in the policy iteration algorithm
policyEvaluation :: (MDP a action c) -> [[[Double]]] -> [[[action]]] -> Double -> [[[Double]]]
-- (map (\q -> zipWith (+) [0..((length q) - 1)] q) (map (\x -> replicate (length (q !! x)) x) [0..((length q - 1))]))
policyEvaluation mdp@(MDP state acts trans rews acc getUtils) utilities policy discount = mapToAllWithRowColZ (\z -> \col -> \row -> singlePolicyUpdate mdp [z, row, col] utilities policy discount) (replicate (length utilities) (replicate (length . head utilities) [0..((length $ utilities !! 0 !! 0) - 1)]))

-- Generates a policy given utilities
generatePolicy :: (MDP a action c) -> [[[Double]]] -> Double -> [[[action]]]
generatePolicy mdp@(MDP state acts trans rews acc getUtils) utilities discount = mapToAllWithRowColZ (\z -> \col -> \row -> (argmax acts (expectedUtility mdp [z, row, col] utilities))) (replicate (length utilities) (replicate (length . head utilities) [0..((length $ utilities !! 0 !! 0) - 1)]))

-- Given an MDP, utilities, and discount, this function constructs the next iteration of the same
utilityIteration :: ((MDP a action c), [[[Double]]], Double) -> ((MDP a action c), [[[Double]]], Double)
utilityIteration (mdp@(MDP state acts trans rews acc getUtils), utilities, discount) = (mdp, utilityEvaluation mdp utilities discount, discount)

-- Given an MDP, utilities, policy, and discount, this function constructs the next iteration of the same
policyIteration :: ((MDP a action c), [[[Double]]], [[[action]]], Double) -> ((MDP a action c), [[[Double]]], [[[action]]], Double)
policyIteration (mdp@(MDP state acts trans rews acc getUtils), utilities, policy, discount) = (mdp, updatedUtilities, (generatePolicy mdp updatedUtilities discount), discount)
                                                    where updatedUtilities = policyEvaluation mdp utilities policy discount

-- Gets the converged policy
finishPolicyCheck :: Eq action => [((MDP a action c), [[[Double]]], [[[action]]], Double)] -> Int -> Int -> ((MDP a action c), [[[Double]]], [[[action]]], Double)
finishPolicyCheck (x:xs) orig num | num == 0 && (samePolicy x (head xs)) = x
                                  | num > 0 && (samePolicy x (head xs)) = finishPolicyCheck xs orig (num - 1)
                                  | otherwise = finishPolicyCheck xs orig orig
                                  where samePolicy (a, b, pol1, c) (d, e, pol2, f) = (pol1 == pol2)

-- Gets the final iteration check
finishIterationCheck :: [((MDP a action c), [[[Double]]], Double)] -> Double -> ((MDP a action c), [[[Double]]], Double)
finishIterationCheck (x@(mdp, utils, discount):xs) epsilon | ((maxDifference x (head xs)) <= (epsilon*(1 - discount)/discount)) = (head xs)
                                                           | otherwise = finishIterationCheck xs epsilon
                                                           where maxDifference (a, util1, b) (c, util2, d) = maximum (zipWith (\a -> \b -> maxD2D a b) util1 util2)
                                                                 maxD2D util1 util2 = maximum . fmap maximum $ zipWith (\x -> \y -> abs <$> zipWith (-) x y) util1 util2

--------------------------------------------------------------------------------------------------------
-------------------------------------------- Main Program ----------------------------------------------
--------------------------------------------------------------------------------------------------------

main = do   putStrLn $ ""
            if (finalValPolicy /= finalPolPolicy) then do
                putStrLn $ "Policy iteration utilities: "
                putStrLn $ showGrid3D ((++ " | ") . show) finalPolUtils
            else
                putStr ""
            putStrLn $ "Value iteration utilities: "
            putStrLn $ showGrid3D ((++ " | ") . show) finalValUtils
            putStrLn $ "Policy final: "
            putStrLn . showGrid3D . displayAction $ finalPolPolicy
            if (finalValPolicy /= finalPolPolicy) then do
                putStrLn $ "Value final: "
                putStrLn . showGrid3D . displayAction $ finalValPolicy
            else
                putStr ""

                where thisMDP = wumpusMaze
                      thisPolicy = wumpusSamplePolicy
                      theseUtils = wumpusUtilities
                      polMDPs = iterate policyIteration (thisMDP, theseUtils, generatePolicy thisMDP theseUtils discount, discount)
                      valMDPs = iterate utilityIteration (thisMDP, theseUtils, discount)
                      (finalValMDP, finalValUtils, valDiscount) = finishIterationCheck valMDPs 1e-6
                      finalValPolicy = generatePolicy finalValMDP finalValUtils discount
                      (finalPolMDP, finalPolUtils, finalPolPolicy, polDiscount) = finishPolicyCheck polMDPs 5000 5000
                      discount = 1
