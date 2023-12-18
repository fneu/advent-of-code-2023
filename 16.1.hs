import Data.Char (isSpace, isDigit)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (nub)
import Debug.Trace (trace)

charAt :: [String] -> Position -> Char
charAt lines (Position row column)
  | row < 0 || column < 0 = 'X'
  | row >= length lines = 'X'
  | column >= length (head lines) = 'X'
  | otherwise = lines !! row !! column

data Position = Position { row :: Int, column :: Int } deriving (Show, Eq)

data Direction = North | East | South | West deriving (Show, Eq)

data Beam = Beam { position :: Position, direction :: Direction } deriving (Show, Eq)

northOf :: Position -> Position
northOf (Position row column) = Position (row - 1) column

eastOf :: Position -> Position
eastOf (Position row column) = Position row (column + 1)

southOf :: Position -> Position
southOf (Position row column) = Position (row + 1) column

westOf :: Position -> Position
westOf (Position row column) = Position row (column - 1)

beamed :: [String] -> [Beam] -> Beam -> [Beam]
beamed lines previous current
  | current `elem` previous = previous
  | otherwise =
      case direction current of
        North ->
          case charAt lines (position current) of
            'X' -> previous
            '.' -> current: toNorth
            '-' -> current : lbeamed lines (current : toEast) (Beam (westOf (position current)) West)
            '|' -> current : toNorth
            '/' -> current : toEast
            '\\' -> current : toWest
        East ->
          case charAt lines (position current) of
            'X' -> previous
            '.' -> current : toEast
            '-' -> current : toEast
            '|' -> current : lbeamed lines (current : toNorth) (Beam (southOf (position current)) South)
            '/' -> current : toNorth
            '\\' -> current : toSouth
        South ->
          case charAt lines (position current) of
            'X' -> previous
            '.' -> current : toSouth
            '-' -> current : lbeamed lines (current : toWest) (Beam (eastOf (position current)) East)
            '|' -> current : toSouth
            '/' -> current : toWest
            '\\' -> current : toEast
        West ->
          case charAt lines (position current) of
            'X' -> previous
            '.' -> current : toWest
            '-' -> current : toWest
            '|' -> current : lbeamed lines (current : toSouth) (Beam (northOf (position current)) North)
            '/' -> current : toSouth
            '\\' -> current : toNorth
  where
    toNorth = lbeamed lines (current : previous) $ Beam (northOf (position current)) North
    toEast = lbeamed lines (current : previous) $ Beam (eastOf (position current)) East
    toSouth = lbeamed lines (current : previous) $ Beam (southOf (position current)) South
    toWest = lbeamed lines (current : previous) $ Beam (westOf (position current)) West


lbeamed :: [String] -> [Beam] -> Beam -> [Beam]
-- lbeamed lines previous current = trace (show current ++ ": " ++ show (charAt lines (position current)) ++ " do we know it? -> " ++ show (current `elem` previous) ++ ": " ++ show (length previous)) beamed lines previous current
lbeamed = beamed

main :: IO ()
main = do
  input <- fmap lines getContents
  print $ length $ nub $ map position $ lbeamed input [] (Beam (Position 0 0) East)
