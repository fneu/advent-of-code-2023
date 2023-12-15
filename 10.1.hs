import Data.Maybe (fromMaybe, listToMaybe)

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

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East = West
opposite West = East

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

meetDistance :: [String] -> [State] -> Int -> Int
meetDistance lines states distance
  | position ( head states ) == position ( last states ) = distance
  | otherwise = meetDistance lines (map (walk lines) states) (distance + 1)

main :: IO ()
main = do
  input <- fmap lines getContents
  let s = findS input
  let directions = possibleDirections input s
  print $ meetDistance input [walk input (uncurry State s (head directions)), walk input (uncurry State s (last directions))] 1
