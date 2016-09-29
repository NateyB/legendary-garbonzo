module Russell where

russellUtilities = NDimensionalGrid [
            OneDimensionalGrid [0, 0, 0, 0],
            OneDimensionalGrid [0, 0, 0, 0],
            OneDimensionalGrid [0, 0, 0, 0]
            ]

russellSamplePolicy = NDimensionalGrid [
        OneDimensionalGrid [North, East, East, North],
        OneDimensionalGrid [North, West, West, North],
        OneDimensionalGrid [North, West, West, South]
        ]

displayAction :: RussellAction -> String
displayAction North = "^"
displayAction West  = "<"
displayAction East  = ">"
displayAction South = "v"

---------------------------------------- Russell 3x4 World ---------------------------------------------
data RussellPanel = Usable Bool
               | Terminal Double
               deriving (Eq, Show);

russellWorld = [
       [Usable True, Usable True, Usable True, Terminal (1.0)],
       [Usable True, Usable False, Usable True, Terminal (0 - 1.0)],
       [Usable True, Usable True, Usable True, Usable True]
       ]

data RussellAction = North
               | West
               | East
               | South
               deriving (Eq, Show);

russellActions = [North, West, East, South]

russellAccessor :: [[RussellPanel]] -> [Int] -> RussellPanel
russellAccessor state coords = state !! (coords !! 0) !! (coords !! 1)

russellUtils :: [[RussellPanel]] -> [Int] -> [[Double]] -> [Double]
russellUtils state coords utils = case (state !! row !! col) of
       (Usable False) -> [0, 0, 0, 0, 0]
       _ -> [if withinBounds (row - 1) col then utils !! (row - 1) !! col else 0,
                      if withinBounds row (col - 1) then utils !! row !! (col - 1) else 0,
                      if withinBounds row (col + 1) then utils !! row !! (col + 1) else 0,
                      if withinBounds (row + 1) col then utils !! (row + 1) !! col else 0,
                      utils !! row !! col]
       where
           row = coords !! 0
           col = coords !! 1
           withinBounds row col = (row >= 0) && (row < (length state)) && (col >= 0) && (col < (length (state !! row)) && (state !! row !! col /= (Usable False)))

russellTransition :: [[RussellPanel]] -> RussellAction -> [Int] -> [Double]
russellTransition state act coords
       | (not (state !! row !! col == (Usable True))) = [0, 0, 0, 0, 0]
       | (act == North) = let x = [if withinBounds (row - 1) col then main else 0,
                                   if withinBounds row (col - 1) then side else 0,
                                   if withinBounds row (col + 1) then side else 0,
                                   0,
                                   1 - (sum . init) x] in x
       | (act == West)  = let x = [if withinBounds (row - 1) col then side else 0,
                                   if withinBounds row (col - 1) then main else 0,
                                   0,
                                   if withinBounds (row + 1) col then side else 0,
                                   1 - (sum . init) x] in x
       | (act == East)  = let x = [if withinBounds (row - 1) col then side else 0,
                                   0,
                                   if withinBounds row (col + 1) then main else 0,
                                   if withinBounds (row + 1) col then side else 0,
                                   1 - (sum . init) x] in x
       | (act == South) = let x = [0,
                                   if withinBounds row (col - 1) then side else 0,
                                   if withinBounds row (col + 1) then side else 0,
                                   if withinBounds (row + 1) col then main else 0,
                                   1 - (sum . init) x] in x
       where
           row = coords !! 0
           col = coords !! 1
           withinBounds row col = ((row >= 0) && (row < (length state)) && (col >= 0) && (col < (length (state !! row))) && (state !! row !! col /= (Usable False)))
           main = 0.8
           side = 0.1

russellReward :: RussellPanel -> Double
russellReward (Usable True) = (0 - 0.04)
russellReward (Terminal x) = x
russellReward _ = 0

russell3x4 = MDP russellWorld russellActions russellAccessor russellUtils russellTransition russellReward
