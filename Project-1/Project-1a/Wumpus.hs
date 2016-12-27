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


wumpusSamplePolicy = NDimensionalGrid [
        NDimensionalGrid [
            OneDimensionalGrid [West, South, South, North],
            OneDimensionalGrid [West, North, East, South],
            OneDimensionalGrid [South, West, East, South],
            OneDimensionalGrid [West, North, West, West]
        ],

        NDimensionalGrid [
            OneDimensionalGrid [West, North, North, East],
            OneDimensionalGrid [West, East, East, East],
            OneDimensionalGrid [West, West, North, South],
            OneDimensionalGrid [South, East, East, West]
        ],

        NDimensionalGrid [
            OneDimensionalGrid [South, West, South, South],
            OneDimensionalGrid [East, South, South, North],
            OneDimensionalGrid [North, East, West, North],
            OneDimensionalGrid [East, South, West, East]
        ],

        NDimensionalGrid [
            OneDimensionalGrid [East, West, East, West],
            OneDimensionalGrid [North, North, East, North],
            OneDimensionalGrid [North, South, North, South],
            OneDimensionalGrid [North, East, West, North]]
        ] :: NDimensionalGrid WumpusAction

------------------------------------------ Wumpus World ------------------------------------------------

wumpusWorld = let {pit1 = Pit (0.95*cOD); pit2 = Pit (0.15*cOD); pit3 = Pit (0.25*cOD); cOD = (-1.0)} in
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

wumpusUtilities :: NDimensionalGrid Double
wumpusUtilities = fmap (const 0) wumpusWorld

displayAction :: WumpusAction -> String
displayAction North  = "^"
displayAction West   = "<"
displayAction East   = ">"
displayAction South  = "v"
displayAction Pickup = "P"

wumpusActions = [North, West, East, South, Pickup]

wumpusAccessor :: [[[WumpusPanel]]] -> [Int] -> WumpusPanel
wumpusAccessor state coords = state !! head coords !! (coords !! 1) !! (coords !! 2)

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
        Pit x -> fmap (* (1 - abs x)) [if withinBounds zIn (row - 1) col then utils !! zIn !! (row - 1) !! col else 0,
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
            zIn = head coords
            row = coords !! 1
            col = coords !! 2
            withinBounds zIn row col = row >= 0 && row < length (state !! zIn) && col >= 0 && col < length (state !! zIn !! row) -- && (state !! zIn !! row !! col /= (Usable False)))

-- Vector of the possible transition probabilities
wumpusTransition :: NDimensionalGrid WumpusPanel -> WumpusAction -> [Coord] -> [Double]
wumpusTransition state act coords
        | isTerminal (state !#! coords) = [0, 0, 0, 0, 0, 0]
        | state !#! coords == HasGold = case act of
            North  ->  [0, side, side, 0, 0, main]
            West   ->   [0, main, 0, side, 0, side]
            East   ->   [0, 0, main, side, 0, side]
            South  ->  [0, side, side, main, 0, 0]
            Pickup -> [0, 0, 0, 0, 1, 0]
        | act == North = let x = [if withinBounds zIn (row - 1) col then main else 0,
                                    if withinBounds zIn row (col - 1) then side else 0,
                                    if withinBounds zIn row (col + 1) then side else 0,
                                    0,
                                    0,
                                    1 - (sum . init) x] in x
        | act == West  = let x = [if withinBounds zIn (row - 1) col then side else 0,
                                    if withinBounds zIn row (col - 1) then main else 0,
                                    0,
                                    if withinBounds zIn (row + 1) col then side else 0,
                                    0,
                                    1 - (sum . init) x] in x
        | act == East  = let x = [if withinBounds zIn (row - 1) col then side else 0,
                                    0,
                                    if withinBounds zIn row (col + 1) then main else 0,
                                    if withinBounds zIn (row + 1) col then side else 0,
                                    0,
                                    1 - (sum . init) x] in x
        | act == South = let x = [0,
                                    if withinBounds zIn row (col - 1) then side else 0,
                                    if withinBounds zIn row (col + 1) then side else 0,
                                    if withinBounds zIn (row + 1) col then main else 0,
                                    0,
                                    1 - (sum . init) x] in x
        | act == Pickup = [0, 0, 0, 0, 1, 0]
        where
            zIn = head coords
            row = coords !! 1
            col = coords !! 2
            isTerminal state = case state of
                Terminal _ -> True
                _          -> False
            withinBounds zIn row col = row >= 0 && row < length (state !! zIn) && col >= 0 && col < length (state !! zIn !! row) -- && (state !! zIn !! row !! col /= (Usable False)))
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


wumpusMaze = MDP wumpusWorld wumpusActions wumpusAccessor wumpusUtils wumpusTransition wumpusReward
