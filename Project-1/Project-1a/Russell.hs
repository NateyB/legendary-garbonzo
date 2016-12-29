module Russell where
import           MDPs

------------------------------ Russell 3x4 World -------------------------------
data RussellPanel = Usable Bool
              | Terminal Double
              deriving (Eq, Show);

data RussellAction = North
               | West
               | East
               | South
               deriving (Eq);

instance Show RussellAction where
    show North = "^"
    show West  = "<"
    show East  = ">"
    show South = "v"


doesExist :: NDimensionalGrid RussellPanel -> [Coord] -> Bool
doesExist (OneDimensionalGrid items) [column] = 0 <= column
    && column < length items && items !! column /= Usable False

doesExist (NDimensionalGrid items) [row, column] = 0 <= row
    && row < length items && doesExist (items !! row) [column]

doesExist _ _ = error "Dimensionality mismatch in RussellWorld existence check"


-------------------------- MDP Component Definitions ---------------------------
russellWorld :: NDimensionalGrid RussellPanel
russellWorld = let {open = Usable True; wall = Usable False;
                    good = Terminal 1.0; dead = Terminal (-1.0)} in
    NDimensionalGrid [
        OneDimensionalGrid [open, open, open, good],
        OneDimensionalGrid [open, wall, open, dead],
        OneDimensionalGrid [open, open, open, open]
    ]

russellActions :: NDimensionalGrid RussellPanel -> [Coord] -> [RussellAction]
russellActions state = const [North, West, East, South]

russellTransition ::
    NDimensionalGrid RussellPanel
    -> RussellAction
    -> [Coord]
    -> [STP]
russellTransition state act [row, col]
       | state !#! [row,col] /= Usable True = []

       | act == North = let x = ([row, col], distRemainder x) : filter guardMove
                                    [([row - 1, col], main),
                                 ([row, col - 1], side),
                                 ([row, col + 1], side)] in x

       | act == West  = let x = ([row, col], distRemainder x) : filter guardMove
                                    [([row - 1, col], side),
                                   ([row, col - 1], main),
                                   ([row + 1, col], side)] in x

       | act == East  = let x = ([row, col], distRemainder x) : filter guardMove
                                    [([row - 1, col], side),
                                   ([row, col + 1], main),
                                   ([row + 1, col], side)] in x
       | act == South = let x = ([row, col], distRemainder x) : filter guardMove
                                    [([row, col - 1], side),
                                    ([row, col + 1], side),
                                    ([row + 1, col], main)] in x
       where
           guardMove (coords, _) = doesExist state coords
           distRemainder dist = 1 - (sum . tail . fmap snd) dist
           main = 0.8
           side = 0.1

russellReward :: RussellPanel -> Double
russellReward (Usable True) = negate 0.04
russellReward (Terminal x)  = x
russellReward _             = 0

russell3x4 :: MDP RussellPanel RussellAction
russell3x4 = MDP {state = russellWorld,
                  actions = russellActions,
                  transition = russellTransition,
                  reward = russellReward}
