import           MDPs
import           Russell
import           Wumpus

--------------------------------------------------------------------------------------------------------
-------------------------------------------- Main Program ----------------------------------------------
--------------------------------------------------------------------------------------------------------
showPolicyOutcome :: Show a => Eq a => NDimensionalGrid a -> NDimensionalGrid a -> NDimensionalGrid Double -> NDimensionalGrid Double -> String
showPolicyOutcome valPolicy polPolicy valUtils polUtils
    | valPolicy /= polPolicy = concat [pUPreface, pUDisplay, showUtilitiesTail, policyPreface, policyDisplay, showPolicyTail]
    | otherwise = showUtilitiesTail ++ showPolicyTail
        where
            pUPreface = "\nPolicy iteration utilities: \n"
            pUDisplay = showGrid show polUtils
            showUtilitiesTail = "\nValue iteration utilities: \n" ++ showGrid show valUtils

            policyPreface = "\nValue iteration final policy: \n"
            policyDisplay = showGrid show valPolicy
            showPolicyTail = "\nPolicy iteration final policy: \n" ++ showGrid show polPolicy

main :: IO ()
main = putStrLn $ showPolicyOutcome valPolicy polPolicy valUtils polUtils
            where thisMDP = russell3x4
                  (valPolicy, valUtils) = performValueIteration thisMDP discount epsilon
                  (polPolicy, polUtils) = performPolicyIteration thisMDP discount replication
                  discount = 1
                  epsilon = 0
                  replication = 2
