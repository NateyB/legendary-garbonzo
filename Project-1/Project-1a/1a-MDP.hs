import           MDPs
-- Import the MDP that you which to solve
-- import           Russell
import           Wumpus

--------------------------------------------------------------------------------
--------------------------------- Main Program ---------------------------------
--------------------------------------------------------------------------------

-- |This function takes in two policies and two sets of utiltiies, and
-- displays one of the sets of utilities and the policy that it
-- generates if the two policies are the same. If the policies provided
-- differ, then it presents both policies and both sets of utilities
-- that generated them.
showPolicyOutcome :: Show a => Eq a
    => NDimensionalGrid a
    -> NDimensionalGrid a
    -> NDimensionalGrid Double
    -> NDimensionalGrid Double
    -> String
showPolicyOutcome valPolicy polPolicy valUtils polUtils
    | valPolicy /= polPolicy = concat
        [pUPreface, pUDisplay, showUtilitiesTail,
        policyPreface, policyDisplay, showPolicyTail]

    | otherwise = showUtilitiesTail ++ showPolicyTail
        where
            pUPreface = "\nPolicy iteration utilities: \n"
            pUDisplay = showGrid show polUtils
            showUtilitiesTail = "\nValue iteration utilities: \n"
                ++ showGrid show valUtils

            policyPreface = "\nValue iteration final policy: \n"
            policyDisplay = showGrid show valPolicy
            showPolicyTail = "\nPolicy iteration final policy: \n"
                ++ showGrid show polPolicy

main :: IO ()
main = putStrLn $ showPolicyOutcome valPolicy polPolicy valUtils polUtils
            where
                (valPolicy, valUtils) = performValueIteration thisMDP
                    discount epsilon
                (polPolicy, polUtils) = performPolicyIteration thisMDP
                    discount replication

                thisMDP = wumpusMaze
                discount = 1
                epsilon = 0
                replication = 2
