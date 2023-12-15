import Data.Maybe (fromMaybe, listToMaybe)
import Data.List

data Direction = North | South | East | West
  deriving (Show, Eq)

data State = State
  { row :: Int,
    column :: Int,
    origin :: Direction
  }
  deriving (Show)

position :: State -> (Int, Int)
position state = (row state, column state)

findS :: [String] -> (Int, Int)
findS lines =
  let
    flattened =
      [ ( (line, col), c )
      | (line, str) <- zip [0..] lines ,
      (col, c) <- zip [0..] str ]
  in
    fst . head . filter ((== 'S') . snd) $ flattened

-- safe indexing, returns '.' if out of bounds
charAt :: [String] -> Int -> Int -> Char
charAt _ (-1) _ = '.'
charAt _ _ (-1) = '.'
charAt lines row column =
  let line = fromMaybe "" $ listToMaybe $ drop row lines in
  fromMaybe '.' $ listToMaybe $ drop column line

connected :: (Direction, Char) -> Bool
connected (North, c) = c `elem` "|7F"
connected (South, c) = c `elem` "|JL"
connected (East, c) = c `elem` "-7J"
connected (West, c) = c `elem` "-FL"

possibleDirections :: [String] -> (Int, Int) -> [Direction]
possibleDirections lines (row, column) =
  let
    char = charAt lines row column
  in
  case char of
    'S' -> map fst $ filter connected $ zip [North, South, East, West] [north, south, east, west]
    '|' -> [North, South]
    '-' -> [East, West]
    'L' -> [North, East]
    'J' -> [North, West]
    '7' -> [South, West]
    'F' -> [South, East]
    _ -> []
    where
      north = charAt lines (row - 1) column
      south = charAt lines (row + 1) column
      east = charAt lines row (column + 1)
      west = charAt lines row (column - 1)

walk :: [String] -> State -> State
walk lines state
  | nextDirection == North = State (row state - 1) (column state) South
  | nextDirection == South = State (row state + 1) (column state) North
  | nextDirection == East = State (row state) (column state + 1) West
  | nextDirection == West = State (row state) (column state - 1) East
  where nextDirection = head $ filter (/= origin state) $ possibleDirections lines (row state, column state)

classify :: [String] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> State -> ([(Int, Int)], [(Int, Int)], [(Int, Int)])
classify lines visited left right state =
  let
    char = charAt lines (row state) (column state)
  in
    case (char, origin state) of
      ('S', _) -> (visited, left, right)
      ('|', North) -> classify lines newVisited (east:left) (west:right) next
      ('|', South) -> classify lines newVisited (west:left) (east:right) next
      ('-', East) -> classify lines newVisited (south:left) (north:right) next
      ('-', West) -> classify lines newVisited (north:left) (south:right) next
      ('L', North) -> classify lines newVisited left (west:south:right) next
      ('L', East) -> classify lines newVisited (south:west:left) right next
      ('J', North) -> classify lines newVisited (east:south:left) right next
      ('J', West) -> classify lines newVisited left (south:east:right) next
      ('7', South) -> classify lines newVisited left (east:north:right) next
      ('7', West) -> classify lines newVisited (north:east:left) right next
      ('F', South) -> classify lines newVisited (west:north:left) right next
      ('F', East) -> classify lines newVisited left (north:west:right) next
      where
        newVisited = (row state, column state) : visited
        north = (row state - 1, column state)
        south = (row state + 1, column state)
        east = (row state, column state + 1)
        west = (row state, column state - 1)
        next = walk lines state

grow :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
grow visited inside
  | length newInside == length inside = inside
  | otherwise = grow visited newInside
  where
    newInside = filter (`notElem` visited) $ nub $ concatMap (\(row, column) -> [(row, column), (row - 1, column), (row + 1, column), (row, column - 1), (row, column + 1)]) inside

main :: IO ()
main = do
  input <- fmap lines getContents
  let s = findS input
  let startDirection = head $ possibleDirections input s
  let (visited, left, right) = classify input [s] [] [] $ walk input $ uncurry State s startDirection
  let validLeft = filter (`notElem` visited) $ nub left
  let validRight = filter (`notElem` visited) $ nub right
  let inside = grow visited (if length validLeft > length validRight then validRight else validLeft)
  print $ length inside
