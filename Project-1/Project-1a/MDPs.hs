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
