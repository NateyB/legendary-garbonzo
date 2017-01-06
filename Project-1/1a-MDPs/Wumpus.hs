module Wumpus where
import           MDPs

--------------------------------- Wumpus World ---------------------------------
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
                deriving (Eq);

instance Show WumpusAction where
    show North  = "^"
    show West   = "<"
    show East   = ">"
    show South  = "v"
    show Pickup = "P"

doesExist :: NDimensionalGrid WumpusPanel -> [Coord] -> Bool
doesExist (OneDimensionalGrid items) [column] = 0 <= column
    && column < length items

doesExist (NDimensionalGrid items) (row:rows) = 0 <= row
    && row < length items && doesExist (items !! row) rows

doesExist _ _ = error "Dimensionality mismatch in WumpusWorld existence check"

-------------------------- MDP Component Definitions ---------------------------
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
wumpusActions state = const [North, West, East, South, Pickup]

wumpusTransition ::
    NDimensionalGrid WumpusPanel
    -> WumpusAction
    -> [Coord]
    -> [STP]
wumpusTransition state act coords@[zIn, row, col]
        | isTerminal (state !#! coords) = []

        | state !#! coords == HasImmunity
              = wumpusTransition state act [zIn + 2, row, col]

        | act == North = let x = (coords, distRemainder x) : filter guardMove
                                    [([zIn, row - 1, col], main),
                                    ([zIn, row, col - 1], side),
                                    ([zIn, row, col + 1], side)] in x

        | act == West  = let x = (coords, distRemainder x) : filter guardMove
                                    [([zIn, row - 1, col], side),
                                    ([zIn, row, col - 1], main),
                                    ([zIn, row + 1, col], side)] in x

        | act == East  = let x = (coords, distRemainder x) : filter guardMove
                                    [([zIn, row - 1, col], side),
                                    ([zIn, row, col + 1], main),
                                    ([zIn, row + 1, col], side)] in x

        | act == South = let x = (coords, distRemainder x) : filter guardMove
                                    [([zIn, row, col - 1], side),
                                    ([zIn, row, col + 1], side),
                                    ([zIn, row + 1, col], main)] in x

        | act == Pickup && state !#! coords == HasGold
              = [([zIn + 1, row, col], 1)]

        | act == Pickup = [(coords, 1)]

        where
            guardMove (coords, _) = doesExist state coords
            distRemainder dist = 1 - (sum . tail . fmap snd) dist
            isTerminal state = case state of
               Terminal _ -> True
               _          -> False
            main = 0.8
            side = (1 - main)/2

wumpusReward :: WumpusPanel -> Double
wumpusReward panel = case panel of
                Open         -> stepCost
                (Terminal x) -> x
                (Pit y)      -> y + stepCost
                _            -> stepCost
                where
                    stepCost = negate 0.04

{-|
  This MDP is a tad more complicated than the russell maze. In this MDP,
  the player has two boolean variables that dictate states: Whether the
  player is immune to the wumpus and whether the player has the gold. The
  only two terminal states in this maze are being killed by the wumpus and
  getting the gold to the exit. If the player picks up immunity, then the
  player cannot be harmed by the wumpus. There are three pits which damage the
  player.

  This MDP is implemented as a grid with 4 layers:
  1) No immunity & no gold,
  2) No immunity & gold,
  3) Immunity & no gold,
  4) Immunity & gold

  Executive decrees about the rules of this MDP:
  1) Pickup can occur anywhere, but only changes anything in the HasGold square.
  2) Immunity is picked up automatically.
  3) Any movement action can occur anywhere, but (like in the russell maze),
     the player stays put with what would otherwise be the probability of
     moving if an obstacle is in the way.

-}
wumpusMaze = MDP {state = wumpusWorld,
                  actions = wumpusActions,
                  transition = wumpusTransition,
                  reward = wumpusReward}
