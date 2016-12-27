module Russell where
import           MDPs

data RussellAction = North
               | West
               | East
               | South
               deriving (Eq, Show);

displayAction :: RussellAction -> String
displayAction North = "^"
displayAction West  = "<"
displayAction East  = ">"
displayAction South = "v"

---------------------------------------- Russell 3x4 World ---------------------------------------------
data RussellPanel = Usable Bool
               | Terminal Double
               deriving (Eq, Show);

russellWorld :: NDimensionalGrid RussellPanel
russellWorld = NDimensionalGrid [
           OneDimensionalGrid [Usable True, Usable True, Usable True, Terminal 1.0],
           OneDimensionalGrid [Usable True, Usable False, Usable True, Terminal (-1.0)],
           OneDimensionalGrid [Usable True, Usable True, Usable True, Usable True]{-,
           OneDimensionalGrid [Usable True, Usable True, Usable True, Usable True] -- TODO: This last line must be deleted-}
       ]

doesExist :: NDimensionalGrid RussellPanel -> [Coord] -> Bool
doesExist (OneDimensionalGrid items) [column] = 0 <= column && column < length items && items !! column /= Usable False
doesExist (NDimensionalGrid items) [row, column] = 0 <= row && row < length items && doesExist (items !! row) [column]
doesExist _ _ = error "Dimensionality mismatch in existence check in RussellWorld"


russellActions :: NDimensionalGrid RussellPanel -> [Coord] -> [RussellAction]
russellActions state [row, col] = (canNorth . canWest . canEast . canSouth) []
    where
        canEast x | doesExist state [row, col + 1] = East : x | otherwise = x
        canWest x | doesExist state [row, col - 1] = West : x | otherwise = x
        canSouth x | doesExist state [row + 1, col] = South : x | otherwise = x
        canNorth x | doesExist state [row - 1, col] = North : x | otherwise = x

russellTransition :: NDimensionalGrid RussellPanel -> RussellAction -> [Coord] -> [STP]
russellTransition state act [row, col]
       | state !#! [row,col] /= Usable True = replicate 5 ([0,0], 0)

       | act == North = let x = [if doesExist state [row - 1, col] then ([row - 1, col], main) else ([0, 0], 0),
                                 if doesExist state [row, col - 1] then ([row, col - 1], side) else ([0, 0], 0),
                                 if doesExist state [row, col + 1]then ([row, col + 1], side) else ([0, 0], 0),
                                 ([0, 0], 0),
                                 ([row, col], 1 - (sum . init . fmap snd) x)] in x

       | act == West  = let x = [if doesExist state [row - 1, col] then ([row - 1, col], side) else ([0, 0], 0),
                                   if doesExist state [row, col - 1] then ([row, col - 1], main) else ([0, 0], 0),
                                   ([0,0], 0),
                                   if doesExist state [row + 1, col] then ([row + 1, col], side) else ([0, 0], 0),
                                   ([row, col], 1 - (sum . init . fmap snd) x)] in x

       | act == East  = let x = [if doesExist state [row - 1, col] then ([row - 1, col], side) else ([0, 0], 0),
                                   ([0,0], 0),
                                   if doesExist state [row, col + 1] then ([row, col + 1], main) else ([0, 0], 0),
                                   if doesExist state [row + 1, col] then ([row + 1, col], side) else ([0, 0], 0),
                                   ([row, col], 1 - (sum . init . fmap snd) x)] in x
       | act == South = let x = [([0,0], 0),
                                   if doesExist state [row, col - 1] then ([row, col - 1], side) else ([0, 0], 0),
                                   if doesExist state [row, col + 1] then ([row, col + 1], side) else ([0, 0], 0),
                                   if doesExist state [row + 1, col] then ([row + 1, col], main) else ([0, 0], 0),
                                   ([row, col], 1 - (sum . init . fmap snd) x)] in x
       where
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
