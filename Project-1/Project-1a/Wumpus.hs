module Wumpus where
import           MDPs

data WumpusPanel = Open
                | Pit Double
                | HasImmunity
                | HasGold
                | Terminal Double
                deriving (Eq, Show);

data WumpusAction = North
                | West
                | East
                | South
                | Pickup
                deriving (Eq, Show);

displayAction :: WumpusAction -> String
displayAction North  = "^"
displayAction West   = "<"
displayAction East   = ">"
displayAction South  = "v"
displayAction Pickup = "P"

doesExist :: NDimensionalGrid WumpusPanel -> [Coord] -> Bool
doesExist (OneDimensionalGrid items) [column] = 0 <= column && column < length items
doesExist (NDimensionalGrid items) (row:rows) = 0 <= row && row < length items && doesExist (items !! row) rows
doesExist _ _ = error "Dimensionality mismatch in existence check in WumpusWorld"

------------------------------------------ Wumpus World ------------------------------------------------

wumpusWorld :: NDimensionalGrid WumpusPanel
wumpusWorld = let {pit1 = Pit (0.95*cOD); pit2 = Pit (0.15*cOD);
                    pit3 = Pit (0.25*cOD); cOD = (-1.0)} in
    NDimensionalGrid [
        NDimensionalGrid [
            OneDimensionalGrid [Open, HasImmunity, HasGold, Open],
            OneDimensionalGrid [pit1, pit2, Terminal cOD, Open],
            OneDimensionalGrid [Open, Open, Open, Open],
            OneDimensionalGrid [Open, Open, pit3, Open]
        ],

        NDimensionalGrid [
            OneDimensionalGrid [Open, HasImmunity, Open, Open],
            OneDimensionalGrid [pit1, pit2, Terminal cOD, Open],
            OneDimensionalGrid [Open, Open, Open, Open],
            OneDimensionalGrid [Terminal 1, Open, pit3, Open]
        ],

        NDimensionalGrid [
            OneDimensionalGrid [Open, Open, HasGold, Open],
            OneDimensionalGrid [pit1, pit2, Open, Open],
            OneDimensionalGrid [Open, Open, Open, Open],
            OneDimensionalGrid [Open, Open, pit3, Open]
        ],

        NDimensionalGrid [
            OneDimensionalGrid [Open, Open, Open, Open],
            OneDimensionalGrid [pit1, pit2, Open, Open],
            OneDimensionalGrid [Open, Open, Open, Open],
            OneDimensionalGrid [Terminal 1, Open, pit3, Open]
        ]
    ]

wumpusActions :: NDimensionalGrid WumpusPanel -> [Coord] -> [WumpusAction]
wumpusActions state [z, row, col] = (canNorth . canWest . canEast . canSouth) [Pickup]
    where
        canEast x | doesExist state [z, row, col + 1] = East : x | otherwise = x
        canWest x | doesExist state [z, row, col - 1] = West : x | otherwise = x
        canSouth x | doesExist state [z, row + 1, col] = South : x | otherwise = x
        canNorth x | doesExist state [z, row - 1, col] = North : x | otherwise = x

-- -- Will multiply vector: Utility * Probability to get expected utility
-- wumpusUtils :: [[[WumpusPanel]]] -> [Int] -> [[[Double]]] -> [Double]
-- wumpusUtils state coords utils = case (state !! zIn !! row !! col) of
--         Terminal _ -> [0, 0, 0, 0, 0, 0]
--         HasGold -> [0,
--                     utils !! zIn !! row !! (col - 1),
--                     utils !! zIn !! row !! (col + 1),
--                     utils !! zIn !! (row + 1) !! col,
--                     utils !! (zIn + 1) !! row !! col,
--                     utils !! zIn !! row !! col]
--         HasImmunity -> [0,
--                         utils !! (zIn + 2) !! row !! (col - 1),
--                         utils !! (zIn + 2) !! row !! (col + 1),
--                         utils !! (zIn + 2) !! (row + 1) !! col,
--                         utils !! (zIn + 2) !! row !! col,
--                         utils !! (zIn + 2) !! row !! col]
--         Pit x -> fmap (* (1 - abs x)) [if withinBounds zIn (row - 1) col then utils !! zIn !! (row - 1) !! col else 0,
--                                   if withinBounds zIn row (col - 1) then utils !! zIn !! row !! (col - 1) else 0,
--                                   if withinBounds zIn row (col + 1) then utils !! zIn !! row !! (col + 1) else 0,
--                                   if withinBounds zIn (row + 1) col then utils !! zIn !! (row + 1) !! col else 0,
--                                   utils !! zIn !! row !! col,
--                                   utils !! zIn !! row !! col]
--         _ -> [      if withinBounds zIn (row - 1) col then utils !! zIn !! (row - 1) !! col else 0,
--                     if withinBounds zIn row (col - 1) then utils !! zIn !! row !! (col - 1) else 0,
--                     if withinBounds zIn row (col + 1) then utils !! zIn !! row !! (col + 1) else 0,
--                     if withinBounds zIn (row + 1) col then utils !! zIn !! (row + 1) !! col else 0,
--                     utils !! zIn !! row !! col,
--                     utils !! zIn !! row !! col]
--         where
--             zIn = head coords
--             row = coords !! 1
--             col = coords !! 2
--             withinBounds zIn row col = row >= 0 && row < length (state !! zIn) && col >= 0 && col < length (state !! zIn !! row) -- && (state !! zIn !! row !! col /= (Usable False)))
{-
Executive decrees about the rules of this MDP:
1) Pickup can occur anywhere, but only does anything in the HasGold square.
2) Immunity is picked up automatically.
-}


-- Vector of the possible transition probabilities
wumpusTransition :: NDimensionalGrid WumpusPanel -> WumpusAction -> [Coord] -> [STP]
wumpusTransition state act coords@[zIn, row, col]
        | isTerminal (state !#! coords) = replicate 6 ([0, 0, 0], 0)
        | state !#! coords == HasImmunity = wumpusTransition state act [zIn + 2, row, col]
        | act == North = let x = [guardMove [zIn, row - 1, col] main,
                                    guardMove [zIn, row, col - 1] side,
                                    guardMove [zIn, row, col + 1] side,
                                    ([0, 0, 0], 0),
                                    ([0, 0, 0], 0),
                                    (coords, distRemainder x)] in x
        | act == West  = let x = [guardMove [zIn, row - 1, col] side,
                                    guardMove [zIn, row, col - 1] main,
                                    ([0, 0, 0], 0),
                                    guardMove [zIn, row + 1, col] side,
                                    ([0, 0, 0], 0),
                                    (coords, distRemainder x)] in x
        | act == East  = let x = [guardMove [zIn, row - 1, col] side,
                                    ([0, 0, 0], 0),
                                    guardMove [zIn, row, col + 1] main,
                                    guardMove [zIn, row + 1, col] side,
                                    ([0, 0, 0], 0),
                                    (coords, distRemainder x)] in x
        | act == South = let x = [([0, 0, 0], 0),
                                    guardMove [zIn, row, col - 1] side,
                                    guardMove [zIn, row, col + 1] side,
                                    guardMove [zIn, row + 1, col] main,
                                    ([0, 0, 0], 0),
                                    (coords, distRemainder x)] in x
        | act == Pickup && state !#! coords == HasGold = [([0, 0, 0], 0),
            ([0, 0, 0], 0), ([0, 0, 0], 0), ([0, 0, 0], 0),
            ([zIn + 1, row, col], 1), ([0, 0, 0], 0)]
        | act == Pickup = zip (repeat coords) [0, 0, 0, 0, 1.0, 0]
        where
            guardMove :: [Coord] -> Double -> STP
            guardMove coords val
                | doesExist state coords = (coords, val)
                | otherwise = ([0, 0, 0], 0)
            distRemainder dist = 1 - (sum . init . fmap snd) dist
            isTerminal state = case state of
               Terminal _ -> True
               _          -> False
            main = 0.8
            side = 0.1

wumpusReward :: WumpusPanel -> Double
wumpusReward panel = case panel of
                Open         -> stepCost
                (Terminal x) -> x
                (Pit y)      -> y + stepCost
                _            -> stepCost
                where
                    stepCost = negate 0.04


wumpusMaze = MDP {state = wumpusWorld,
                  actions = wumpusActions,
                  transition = wumpusTransition,
                  reward = wumpusReward}
