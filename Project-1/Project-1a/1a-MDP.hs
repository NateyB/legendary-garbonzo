import           MDPs
import           Russell
import           Wumpus

--------------------------------------------------------------------------------------------------------
-------------------------------------------- Main Program ----------------------------------------------
--------------------------------------------------------------------------------------------------------
showPolicyOutcome :: Show a => Eq a => NDimensionalGrid a -> NDimensionalGrid a -> NDimensionalGrid Double -> NDimensionalGrid Double -> String
showPolicyOutcome valPolicy polPolicy valUtils polUtils
    | valPolicy /= polPolicy = pUPreface ++ pUDisplay ++ showUtilitiesTail ++ policyPreface ++ policyDisplay ++ showPolicyTail
    | otherwise = showUtilitiesTail ++ showPolicyTail
        where
            pUPreface = "\nPolicy iteration utilities: \n"
            pUDisplay = showGrid show polUtils
            showUtilitiesTail = "\nValue iteration utilities: \n" ++ showGrid show valUtils

            policyPreface = "\nValue iteration final policy: \n"
            policyDisplay = showGrid show valPolicy
            showPolicyTail = "\nPolicy iteration final policy: \n" ++ showGrid show polPolicy


main :: IO ()
main = do putStrLn $ showPolicyOutcome finalValPolicy finalPolPolicy finalValUtils finalPolUtils
            where thisMDP@MDP{state = curState, actions = getActions} = russell3x4
                  theseUtils = fmap (const 0) curState
                  polMDPs = iterate policyIteration (thisMDP, theseUtils, generatePolicy thisMDP theseUtils, discount)
                  valMDPs = iterate utilityIteration (thisMDP, theseUtils, discount)
                  (finalValMDP, finalValUtils, _) = finishIterationCheck valMDPs 0
                  finalValPolicy = generatePolicy finalValMDP finalValUtils
                  (_, finalPolUtils, finalPolPolicy, _) = finishPolicyCheck polMDPs 25 25
                  discount = 1
