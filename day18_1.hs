import Data.Char (isSpace, isDigit)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.List (sort)
import Debug.Trace (trace)

data Direction = UP | DOWN | LEFT | RIGHT deriving (Show, Eq)

data Section = Section {
  direction :: Direction,
  meters :: Int,
  color :: String
}
  deriving (Show, Eq)

parseLine :: String -> Section
parseLine line = Section direction length color
  where
    direction = case head line of
      'U' -> UP
      'D' -> DOWN
      'L' -> LEFT
      'R' -> RIGHT
    length = read $ takeWhile isDigit $ drop 2 line
    color = filter (`notElem` "()") $ words line !! 2

data Position = Position { row :: Int, column :: Int } deriving (Eq)

instance Show Position where
  show (Position row column) = "(" ++ show row ++ ", " ++ show column ++ ")"

dig :: [Position] -> [Section] -> [Position]
dig visited [] = visited
dig visited (section:next) = case direction section of
  UP ->
      dig (visited++[Position (row start - i) (column start) | i <- [1 .. meters section]]) next
  DOWN ->
      dig (visited++[Position (row start + i) (column start) | i <- [1 .. meters section]]) next
  LEFT ->
      dig (visited++[Position (row start) (column start - i) | i <- [1 .. meters section]]) next
  RIGHT ->
      dig (visited++[Position (row start) (column start + i) | i <- [1 .. meters section]]) next
  where start = last visited

count :: [Position] -> Int -> Int -> Int -> Bool -> Bool -> Int
count border row column length above below
  | column > length = 0
  | onBorder = 1 + count border row (column + 1) length newAbove newBelow
  | newAbove && newBelow = 1 + count border row (column + 1) length newAbove newBelow
  | otherwise = count border row (column + 1) length newAbove newBelow
  where
    onBorder = Position row column `elem` border
    newAbove = if onBorder && Position (row - 1) column `elem` border then not above else above
    newBelow = if onBorder && Position (row + 1) column `elem` border then not below else below

main :: IO ()
main = do
  input <- fmap lines getContents
  let instructions = map parseLine input
  let trench = dig [Position 0 0] instructions
  let topmost = minimum $ map row trench
  let bottommost = maximum $ map row trench
  let leftmost = minimum $ map column trench
  let rightmost = maximum $ map column trench
  let movedTrench = map (\(Position row column) -> Position (row - topmost) (column - leftmost)) trench
  print $ sum $ map (\row -> count movedTrench row 0 (rightmost - leftmost) False False) [0 .. bottommost - topmost]
