import Data.Char (isSpace, isDigit)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.List (sort)
import Debug.Trace (trace)

lossAt :: [String] -> Position -> Maybe Int
lossAt lines (Position row column)
  | row < 0 || column < 0 = Nothing
  | row >= length lines = Nothing
  | column >= length (head lines) = Nothing
  | otherwise = Just $ read [lines !! row !! column]

data Position = Position {row :: Int, column :: Int} deriving (Eq)

instance Show Position where
  show (Position row column) = "(" ++ show row ++ ", " ++ show column ++ ")"

data Direction = North | East | South | West deriving (Show, Eq)

leftTurn :: Direction -> Direction
leftTurn North = West
leftTurn East = North
leftTurn South = East
leftTurn West = South

rightTurn :: Direction -> Direction
rightTurn North = East
rightTurn East = South
rightTurn South = West
rightTurn West = North

oneFurther :: Position -> Direction -> Position
oneFurther (Position row column) North = Position (row - 1) column
oneFurther (Position row column) East = Position row (column + 1)
oneFurther (Position row column) South = Position (row + 1) column
oneFurther (Position row column) West = Position row (column - 1)

data Jump = Jump
  { origin :: Position,
    direction :: Direction,
    straightJumps :: Int,
    totalLoss :: Int,
    history :: [Position]
  }
  deriving (Show, Eq)

instance Ord Jump where
  compare (Jump _ _ _ loss1 _) (Jump _ _ _ loss2 _) = compare loss1 loss2

destination :: Jump -> Position
destination jump = oneFurther (origin jump) (direction jump)

continuations :: [String] -> Jump -> [Jump]
continuations lines jump@(Jump origin direction straights loss history)
  | straights < 4 = catMaybes [toStraight]
  | straights >= 10 = catMaybes [toLeft, toRight]
  | otherwise = catMaybes [toLeft, toRight, toStraight]
  where
    toLeft = case lossAt lines (oneFurther (destination jump) (leftTurn direction)) of
      Nothing -> Nothing
      Just destinationLoss ->
        Just
          Jump
            { origin = destination jump,
              direction = leftTurn direction,
              straightJumps = 1,
              totalLoss = loss + destinationLoss,
              history = destination jump : history
            }
    toRight = case lossAt lines (oneFurther (destination jump) (rightTurn direction)) of
      Nothing -> Nothing
      Just destinationLoss ->
        Just
          Jump
            { origin = destination jump,
              direction = rightTurn direction,
              straightJumps = 1,
              totalLoss = loss + destinationLoss,
              history = destination jump : history
            }
    toStraight = case lossAt lines (oneFurther (destination jump) direction) of
      Nothing -> Nothing
      Just destinationLoss ->
        Just
          Jump
            { origin = destination jump,
              direction = direction,
              straightJumps = straights + 1,
              totalLoss = loss + destinationLoss,
              history = destination jump : history
            }

findPath :: Position -> [String] -> [Jump] -> [Jump] -> Jump
findPath goal lines visited [] = error "No path found"
findPath goal lines visited (candidate : todo)
  | destination candidate == goal = if straightJumps candidate >= 4 then candidate else findPath goal lines (candidate:visited) todo
  | otherwise = case filter
      ( \v ->
          origin v == origin candidate
            && direction v == direction candidate
            && totalLoss v <= totalLoss candidate
            && (straightJumps v >= 4 || straightJumps v == straightJumps candidate)
            && straightJumps v <= straightJumps candidate
      )
      visited of
      [] -> findPath goal lines (candidate : visited) (sort (todo ++ continuations lines candidate))
      _ -> findPath goal lines visited todo

main :: IO ()
main = do
  input <- fmap lines getContents
  print $ findPath (Position (length input -1) (length (head input) -1)) input []
    [Jump {origin = Position 0 (-1), direction = East, straightJumps = 0, totalLoss = 0, history=[]},
    Jump {origin = Position (-1) 0, direction = South, straightJumps = 0, totalLoss = 0, history=[]}]
