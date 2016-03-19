-- MDP is a set of states, actions in those states (here actions are static), a transition model,
-- and a reward. Note that our transition model must also take in coordinates.
-- As a final point of note, the MDP must also pass an accessor so that the coordinates can be used.
data MDP state action stateComponent = MDP state [action] (state -> [Int] -> stateComponent) (state -> [Int] -> [[[Double]]] -> [Double]) (state -> action -> [Int] -> [Double]) (stateComponent -> Double)


--------------------------------------------------------------------------------------------------------
-------------------------------------------- Initialization --------------------------------------------
--------------------------------------------------------------------------------------------------------
russellUtilities = [
            [0, 0, 0, 0],
            [0, 0, 0, 0],
            [0, 0, 0, 0]
            ] :: [[Double]]

russellSamplePolicy = [
        [North, East, East, North],
        [North, West, West, North],
        [North, West, West, South]
        ]

wumpusSamplePolicy = [
        [[West, South, South, North],
        [West, North, East, South],
        [South, West, East, South],
        [West, North, West, West]],

        [[West, North, North, East],
        [West, East, East, East],
        [West, West, North, South],
        [South, East, East, West]],

        [[South, West, South, South],
        [East, South, South, North],
        [North, East, West, North],
        [East, South, West, East]],

        [[East, West, East, West],
        [North, North, East, North],
        [North, South, North, South],
        [North, East, West, North]]
        ]

wumpusUtilities = [
        [[0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 0, 0, 0]],

        [[0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 0, 0, 0]],

        [[0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 0, 0, 0]],

        [[0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 0, 0, 0]]
        ]


--------------------------------------------------------------------------------------------------------
------------------------------------------ Helper utilities --------------------------------------------
--------------------------------------------------------------------------------------------------------
--displayAction :: RussellAction -> String
--displayAction North = "^"
--displayAction West  = "<"
--displayAction East  = ">"
--displayAction South = "v"

displayAction :: WumpusAction -> String
displayAction North = "^"
displayAction West  = "<"
displayAction East  = ">"
displayAction South = "v"
displayAction Pickup = "P"

showGrid :: (a -> String) -> [[a]] -> String; -- Formats a grid of information using the function specified
showGrid f (x:xs) = (concatMap (f) x) ++ "\n" ++ showGrid f xs;
showGrid f _ = ""

showGrid3D :: (a -> String) -> [[[a]]] -> String; -- Formats a grid of information using the function specified
showGrid3D f x = (concatMap ((++ "\n") . (showGrid f)) x)

argmax :: [a] -> (a -> Double) -> a;
argmax (q:qs) f
        | (null qs) = q
        | ((f q) < (f (head qs))) = argmax qs f
        | otherwise = argmax (q:(tail qs)) f

mapToAllWithRowAndCol :: (Int -> Int -> b) -> [[a]] -> [[b]]
mapToAllWithRowAndCol f doubleArray = (map (\row -> (zipWith (f) [0..((length row) - 1)] row)) (map (\x -> replicate (length (doubleArray !! x)) x) [0..((length doubleArray) - 1)]))

mapToAllWithRowColZ :: (Int -> Int -> Int -> b) -> [[[a]]] -> [[[b]]];
mapToAllWithRowColZ f tripleArray = map (\z -> (mapToAllWithRowAndCol (\col -> \row -> (f z col row)) (tripleArray !! z))) [0..((length tripleArray) - 1)]

--------------------------------------------------------------------------------------------------------
-------------------------------------- MDP Solving Utilities -------------------------------------------
--------------------------------------------------------------------------------------------------------

-- Given an MDP with coordinates, calculates the expected utility of the action provided.
expectedUtility :: (MDP a action c) -> [Int] -> [[[Double]]] -> action -> Double
expectedUtility (MDP state acts acc getUtils trans rews) coords utilities act = sum (zipWith (*) (trans state act coords) (getUtils state coords utilities))

-- Updates the utility for a single tile in the value iteration algorithm
singleUtilityUpdate :: (MDP a action c) -> [Int] -> [[[Double]]] -> Double -> Double
singleUtilityUpdate mdp@(MDP state acts acc getUtils trans rews) coords utilities discount = (rews $ acc state coords) + (discount * (maximum (map (expectedUtility mdp coords utilities) acts)))

-- Updates the utility for a single tile in the policy iteration algorithm
singlePolicyUpdate :: (MDP a action c) -> [Int] -> [[[Double]]] -> [[[action]]] -> Double -> Double
singlePolicyUpdate mdp@(MDP state acts acc getUtils trans rews) coords utilities policy discount = (rews $ acc state coords) + (discount * (expectedUtility mdp coords utilities (policy !! (coords !! 0) !! (coords !! 1) !! (coords !! 2))))

-- Updates the utilities for every tile in the value iteration algorithm
utilityEvaluation :: (MDP a action c) -> [[[Double]]] -> Double -> [[[Double]]]
utilityEvaluation mdp@(MDP state acts acc getUtils trans rews) utilities discount = (mapToAllWithRowColZ (\z -> \col -> \row -> singleUtilityUpdate mdp [z, row, col] utilities discount) utilities)

-- Updates the utilities for every tile in the policy iteration algorithm
policyEvaluation :: (MDP a action c) -> [[[Double]]] -> [[[action]]] -> Double -> [[[Double]]]
-- (map (\q -> zipWith (+) [0..((length q) - 1)] q) (map (\x -> replicate (length (q !! x)) x) [0..((length q - 1))]))
policyEvaluation mdp@(MDP state acts acc getUtils trans rews) utilities policy discount = mapToAllWithRowColZ (\z -> \col -> \row -> singlePolicyUpdate mdp [z, row, col] utilities policy discount) (replicate (length utilities) (replicate (length $ utilities !! 0) [0..((length $ utilities !! 0 !! 0) - 1)]))

-- Generates a policy given utilities
generatePolicy :: (MDP a action c) -> [[[Double]]] -> Double -> [[[action]]]
generatePolicy mdp@(MDP state acts acc getUtils trans rews) utilities discount = mapToAllWithRowColZ (\z -> \col -> \row -> (argmax acts (expectedUtility mdp [z, row, col] utilities))) (replicate (length utilities) (replicate (length $ utilities !! 0) [0..((length $ utilities !! 0 !! 0) - 1)]))

-- Given an MDP, utilities, and discount, this function constructs the next iteration of the same
utilityIteration :: ((MDP a action c), [[[Double]]], Double) -> ((MDP a action c), [[[Double]]], Double)
utilityIteration (mdp@(MDP state acts acc getUtils trans rews), utilities, discount) = (mdp, utilityEvaluation mdp utilities discount, discount)

-- Given an MDP, utilities, policy, and discount, this function constructs the next iteration of the same
policyIteration :: ((MDP a action c), [[[Double]]], [[[action]]], Double) -> ((MDP a action c), [[[Double]]], [[[action]]], Double)
policyIteration (mdp@(MDP state acts acc getUtils trans rews), utilities, policy, discount) = (mdp, updatedUtilities, (generatePolicy mdp updatedUtilities discount), discount)
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
                                                                 maxD2D util1 util2 = maximum $ map maximum $ zipWith (\x -> \y -> map abs $ zipWith (-) x y) util1 util2


--------------------------------------------------------------------------------------------------------
---------------------------------------- Definition of MDPs --------------------------------------------
--------------------------------------------------------------------------------------------------------

---------------------------------------- Russell 3x4 World ---------------------------------------------
--data RussellPanel = Usable Bool
--                | Terminal Double
--                deriving (Eq, Show);

--russellWorld = [
--        [Usable True, Usable True, Usable True, Terminal (1.0)],
--        [Usable True, Usable False, Usable True, Terminal (0 - 1.0)],
--        [Usable True, Usable True, Usable True, Usable True]
--        ]

--data RussellAction = North
--                | West
--                | East
--                | South
--                deriving (Eq, Show);

--russellActions = [North, West, East, South]

--russellAccessor :: [[RussellPanel]] -> [Int] -> RussellPanel
--russellAccessor state coords = state !! (coords !! 0) !! (coords !! 1)

--russellUtils :: [[RussellPanel]] -> [Int] -> [[Double]] -> [Double]
--russellUtils state coords utils = case (state !! row !! col) of
--        (Usable False) -> [0, 0, 0, 0, 0]
--        _ -> [if withinBounds (row - 1) col then utils !! (row - 1) !! col else 0,
--                       if withinBounds row (col - 1) then utils !! row !! (col - 1) else 0,
--                       if withinBounds row (col + 1) then utils !! row !! (col + 1) else 0,
--                       if withinBounds (row + 1) col then utils !! (row + 1) !! col else 0,
--                       utils !! row !! col]
--        where
--            row = coords !! 0
--            col = coords !! 1
--            withinBounds row col = (row >= 0) && (row < (length state)) && (col >= 0) && (col < (length (state !! row)) && (state !! row !! col /= (Usable False)))

--russellTransition :: [[RussellPanel]] -> RussellAction -> [Int] -> [Double]
--russellTransition state act coords
--        | (not (state !! row !! col == (Usable True))) = [0, 0, 0, 0, 0]
--        | (act == North) = let x = [if withinBounds (row - 1) col then main else 0,
--                                    if withinBounds row (col - 1) then side else 0,
--                                    if withinBounds row (col + 1) then side else 0,
--                                    0,
--                                    1 - (sum . init) x] in x
--        | (act == West)  = let x = [if withinBounds (row - 1) col then side else 0,
--                                    if withinBounds row (col - 1) then main else 0,
--                                    0,
--                                    if withinBounds (row + 1) col then side else 0,
--                                    1 - (sum . init) x] in x
--        | (act == East)  = let x = [if withinBounds (row - 1) col then side else 0,
--                                    0,
--                                    if withinBounds row (col + 1) then main else 0,
--                                    if withinBounds (row + 1) col then side else 0,
--                                    1 - (sum . init) x] in x
--        | (act == South) = let x = [0,
--                                    if withinBounds row (col - 1) then side else 0,
--                                    if withinBounds row (col + 1) then side else 0,
--                                    if withinBounds (row + 1) col then main else 0,
--                                    1 - (sum . init) x] in x
--        where
--            row = coords !! 0
--            col = coords !! 1
--            withinBounds row col = ((row >= 0) && (row < (length state)) && (col >= 0) && (col < (length (state !! row))) && (state !! row !! col /= (Usable False)))
--            main = 0.8
--            side = 0.1

--russellReward :: RussellPanel -> Double
--russellReward (Usable True) = (0 - 0.04)
--russellReward (Terminal x) = x
--russellReward _ = 0

--russell3x4 = MDP russellWorld russellActions russellAccessor russellUtils russellTransition russellReward

------------------------------------------ Wumpus World ------------------------------------------------
data WumpusPanel = Open
                | Pit Double
                | HasImmunity
                | HasGold
                | Terminal Double
                deriving (Eq, Show);

wumpusWorld = let {pit1 = Pit (0.95*cOD); pit2 = Pit (0.15*cOD); pit3 = Pit (0.25*cOD); cOD = (-1.0)} in [
        [[Open, HasImmunity, HasGold, Open],
        [pit1, pit2, Terminal cOD, Open],
        [Open, Open, Open, Open],
        [Open, Open, pit3, Open]],

        [[Open, HasImmunity, Open, Open],
        [pit1, pit2, Terminal cOD, Open],
        [Open, Open, Open, Open],
        [Terminal 1, Open, pit3, Open]],

        [[Open, Open, HasGold, Open],
        [pit1, pit2, Open, Open],
        [Open, Open, Open, Open],
        [Open, Open, pit3, Open]],

        [[Open, Open, Open, Open],
        [pit1, pit2, Open, Open],
        [Open, Open, Open, Open],
        [Terminal 1, Open, pit3, Open]]
        ]

data WumpusAction = North
                | West
                | East
                | South
                | Pickup
                deriving (Eq, Show);

wumpusActions = [North, West, East, South, Pickup]

wumpusAccessor :: [[[WumpusPanel]]] -> [Int] -> WumpusPanel
wumpusAccessor state coords = state !! (coords !! 0) !! (coords !! 1) !! (coords !! 2)

-- Will multiply vector: Utility * Probability to get expected utility
wumpusUtils :: [[[WumpusPanel]]] -> [Int] -> [[[Double]]] -> [Double]
wumpusUtils state coords utils = case (state !! zIn !! row !! col) of
        Terminal _ -> [0, 0, 0, 0, 0, 0]
        HasGold -> [0,
                    utils !! zIn !! row !! (col - 1),
                    utils !! zIn !! row !! (col + 1),
                    utils !! zIn !! (row + 1) !! col,
                    utils !! (zIn + 1) !! row !! col,
                    utils !! zIn !! row !! col]
        HasImmunity -> [0,
                        utils !! (zIn + 2) !! row !! (col - 1),
                        utils !! (zIn + 2) !! row !! (col + 1),
                        utils !! (zIn + 2) !! (row + 1) !! col,
                        utils !! (zIn + 2) !! row !! col,
                        utils !! (zIn + 2) !! row !! col]
        Pit x -> map (* (1 - abs x)) [if withinBounds zIn (row - 1) col then utils !! zIn !! (row - 1) !! col else 0,
                                  if withinBounds zIn row (col - 1) then utils !! zIn !! row !! (col - 1) else 0,
                                  if withinBounds zIn row (col + 1) then utils !! zIn !! row !! (col + 1) else 0,
                                  if withinBounds zIn (row + 1) col then utils !! zIn !! (row + 1) !! col else 0,
                                  utils !! zIn !! row !! col,
                                  utils !! zIn !! row !! col]
        _ -> [      if withinBounds zIn (row - 1) col then utils !! zIn !! (row - 1) !! col else 0,
                    if withinBounds zIn row (col - 1) then utils !! zIn !! row !! (col - 1) else 0,
                    if withinBounds zIn row (col + 1) then utils !! zIn !! row !! (col + 1) else 0,
                    if withinBounds zIn (row + 1) col then utils !! zIn !! (row + 1) !! col else 0,
                    utils !! zIn !! row !! col,
                    utils !! zIn !! row !! col]
        where
            zIn = coords !! 0
            row = coords !! 1
            col = coords !! 2
            withinBounds zIn row col = ((row >= 0) && (row < (length $ state !! zIn)) && (col >= 0) && (col < (length (state !! zIn !! row)))) -- && (state !! zIn !! row !! col /= (Usable False)))

-- Vector of the possible transition probabilities
wumpusTransition :: [[[WumpusPanel]]] -> WumpusAction -> [Int] -> [Double]
wumpusTransition state act coords
        | isTerminal (state !! zIn !! row !! col) = [0, 0, 0, 0, 0, 0]
        | state !! zIn !! row !! col == HasGold = case act of
            North ->  [0, side, side, 0, 0, main]
            West ->   [0, main, 0, side, 0, side]
            East ->   [0, 0, main, side, 0, side]
            South ->  [0, side, side, main, 0, 0]
            Pickup -> [0, 0, 0, 0, 1, 0]
        | (act == North) = let x = [if withinBounds zIn (row - 1) col then main else 0,
                                    if withinBounds zIn row (col - 1) then side else 0,
                                    if withinBounds zIn row (col + 1) then side else 0,
                                    0,
                                    0,
                                    1 - (sum . init) x] in x
        | (act == West)  = let x = [if withinBounds zIn (row - 1) col then side else 0,
                                    if withinBounds zIn row (col - 1) then main else 0,
                                    0,
                                    if withinBounds zIn (row + 1) col then side else 0,
                                    0,
                                    1 - (sum . init) x] in x
        | (act == East)  = let x = [if withinBounds zIn (row - 1) col then side else 0,
                                    0,
                                    if withinBounds zIn row (col + 1) then main else 0,
                                    if withinBounds zIn (row + 1) col then side else 0,
                                    0,
                                    1 - (sum . init) x] in x
        | (act == South) = let x = [0,
                                    if withinBounds zIn row (col - 1) then side else 0,
                                    if withinBounds zIn row (col + 1) then side else 0,
                                    if withinBounds zIn (row + 1) col then main else 0,
                                    0,
                                    1 - (sum . init) x] in x
        | (act == Pickup) = [0, 0, 0, 0, 1, 0]
        where
            zIn = coords !! 0
            row = coords !! 1
            col = coords !! 2
            isTerminal state = case state of
                Terminal _ -> True
                _ -> False
            withinBounds zIn row col = ((row >= 0) && (row < (length $ state !! zIn)) && (col >= 0) && (col < (length (state !! zIn !! row)))) -- && (state !! zIn !! row !! col /= (Usable False)))
            main = 0.8
            side = 0.1

wumpusReward :: WumpusPanel -> Double
wumpusReward panel = case panel of
                Open -> stepCost
                (Terminal x) -> x
                (Pit y) -> y + stepCost
                _ -> stepCost
                where
                    stepCost = (-0.04)

wumpusMaze = MDP wumpusWorld wumpusActions wumpusAccessor wumpusUtils wumpusTransition wumpusReward


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
            putStrLn $ showGrid3D (displayAction) $ finalPolPolicy
            if (finalValPolicy /= finalPolPolicy) then do
                putStrLn $ "Value final: "
                putStrLn $ showGrid3D (displayAction) $ finalValPolicy
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