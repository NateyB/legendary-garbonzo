module Wumpus where 

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

displayAction :: WumpusAction -> String
displayAction North = "^"
displayAction West  = "<"
displayAction East  = ">"
displayAction South = "v"
displayAction Pickup = "P"

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
