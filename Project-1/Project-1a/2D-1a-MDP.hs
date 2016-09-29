-- This code is used to solve the 2-Dimensional state version of an MDP

-- Given an MDP with coordinates, calculates the expected utility of the action provided.
expectedUtility :: (MDP a action c) -> [Int] -> [[Double]] -> action -> Double
expectedUtility (MDP state acts acc getUtils trans rews) coords utilities act = sum (zipWith (*) (trans state act coords) (getUtils state coords utilities))

singleUtilityUpdate :: (MDP a action c) -> [Int] -> [[Double]] -> Double -> Double
singleUtilityUpdate mdp@(MDP state acts acc getUtils trans rews) coords utilities discount = (rews $ acc state coords) + (discount * (maximum (map (expectedUtility mdp coords utilities) acts)))

singlePolicyUpdate :: (MDP a action c) -> [Int] -> [[Double]] -> [[action]] -> Double -> Double
singlePolicyUpdate mdp@(MDP state acts acc getUtils trans rews) coords utilities policy discount = (rews $ acc state coords) + (discount * (expectedUtility mdp coords utilities (policy !! (coords !! 0) !! (coords !! 1))))

utilityEvaluation :: (MDP a action c) -> [[Double]] -> Double -> [[Double]]
utilityEvaluation mdp@(MDP state acts acc getUtils trans rews) utilities discount = (mapToAllWithRowAndCol (\col -> \row -> singleUtilityUpdate mdp [row, col] utilities discount) utilities)

-- NOTE: Cannot abstract, as their are no clear dimensions.
policyEvaluation :: (MDP a action c) -> [[Double]] -> [[action]] -> Double -> [[Double]]
-- (map (\q -> zipWith (+) [0..((length q) - 1)] q) (map (\x -> replicate (length (q !! x)) x) [0..((length q - 1))]))
policyEvaluation mdp@(MDP state acts acc getUtils trans rews) utilities policy discount = (mapToAllWithRowAndCol (\col -> \row -> singlePolicyUpdate mdp [row, col] utilities policy discount) (replicate (length utilities) [0..((length $ utilities !! 0) - 1)])

generatePolicy :: (MDP a action c) -> [[Double]] -> Double -> [[action]]
generatePolicy mdp@(MDP state acts acc getUtils trans rews) utilities discount = mapToAllWithRowAndCol (\col -> \row -> (argmax acts (expectedUtility mdp [row, col] utilities))) (replicate (length utilities) [0..((length $ utilities !! 0) - 1)])

utilityIteration :: ((MDP a action c), [[Double]], Double) -> ((MDP a action c), [[Double]], Double)
utilityIteration (mdp@(MDP state acts acc getUtils trans rews), utilities, discount) = (mdp, utilityEvaluation mdp utilities discount, discount)

policyIteration :: ((MDP a action c), [[Double]], [[action]], Double) -> ((MDP a action c), [[Double]], [[action]], Double)
policyIteration (mdp@(MDP state acts acc getUtils trans rews), utilities, policy, discount) = (mdp, updatedUtilities, (generatePolicy mdp updatedUtilities discount), discount)
                                                    where updatedUtilities = policyEvaluation mdp utilities policy discount

finishPolicyCheck :: Eq action => [((MDP a action c), [[Double]], [[action]], Double)] -> Int -> Int -> ((MDP a action c), [[Double]], [[action]], Double)
finishPolicyCheck (x:xs) orig num | num == 0 && (samePolicy x (head xs)) = x
                                  | num > 0 && (samePolicy x (head xs)) = finishPolicyCheck xs orig (num - 1)
                                  | otherwise = finishPolicyCheck xs orig orig
                                  where samePolicy (a, b, pol1, c) (d, e, pol2, f) = (pol1 == pol2)


finishIterationCheck :: [((MDP a action c), [[Double]], Double)] -> Double -> ((MDP a action c), [[Double]], Double)
finishIterationCheck (x@(mdp, utils, discount):xs) epsilon | ((maxDifference x (head xs)) <= (epsilon*(1 - discount)/discount)) = (head xs)
                                                           | otherwise = finishIterationCheck xs epsilon
                                                           where maxDifference (a, util1, b) (c, util2, d) = maximum $ map maximum $ zipWith (\x -> \y -> map abs $ zipWith (-) x y) util1 util2
