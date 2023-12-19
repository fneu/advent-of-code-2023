import Data.Char (isSpace, isDigit)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes, maybeToList)
import Data.List (sort, minimumBy)
import Debug.Trace (trace)
import Data.Ord (comparing)

data Direction = UP | DOWN | LEFT | RIGHT deriving (Show, Eq)

data Section = Section {
  direction :: Direction,
  meters :: Int
}
  deriving (Eq)

instance Show Section where
  show (Section direction length) = show length ++ " " ++ show direction

parseLine :: String -> Section
parseLine line = Section direction length
  where
    color = filter (`notElem` "#()") (words line !! 2)
    direction = case last color of
      '3' -> UP
      '1' -> DOWN
      '2' -> LEFT
      '0' -> RIGHT
    length = read ("0x" ++ init color)

data Position = Position { row :: Int, column :: Int } deriving (Eq)

instance Show Position where
  show (Position row column) = "(" ++ show row ++ ", " ++ show column ++ ")"

data Turn = TOLEFT | TORIGHT deriving (Show, Eq)

corner :: Section -> Section -> Turn
corner a b =
  case (direction a, direction b) of
    (UP, LEFT) -> TOLEFT
    (UP, RIGHT) -> TORIGHT
    (DOWN, LEFT) -> TORIGHT
    (DOWN, RIGHT) -> TOLEFT
    (LEFT, UP) -> TORIGHT
    (LEFT, DOWN) -> TOLEFT
    (RIGHT, UP) -> TOLEFT
    (RIGHT, DOWN) -> TORIGHT
    _ -> error $ "no corner between " ++ show a ++ " and " ++ show b

straighten :: [Section] -> [Section]
straighten [x] = [x]
straighten (x:y:xs)
  | direction x == direction y = straighten $ Section (direction x) (meters x + meters y) : xs
  | otherwise = x : straighten (y:xs)

_cutCorner :: Turn -> [Section] -> (Int, [Section])
_cutCorner positiveTurn sections =
  case findSmallestCut sections of
    Nothing -> (0, sections)
    Just index -> (factor * area, newSections)
      where
        factor = if corner (sections !! index) (sections !! (index + 1)) == positiveTurn then 1 else -1
        area = case factor of
          1 -> peninsulaArea $ drop index sections
          -1 -> actualNegativePeninsulaArea $ drop index sections
        x = sections !! index
        y = sections !! (index + 1)
        z = sections !! (index + 2)
        replacement
          | meters x > meters z = [Section (direction x) (meters x - meters z),y]
          | meters x == meters z = [y]
          | otherwise = [y, Section (direction z) (meters z - meters x)]
        newSections = straighten $ take index sections ++ replacement ++ drop (index + 3) sections


cutCorner :: [Section] -> (Int, [Section])
cutCorner = _cutCorner TORIGHT
-- cutCorner sections = trace ("cutting " ++ show sections ++ " to " ++ show fewerSections ++ ", winning " ++ show cutArea) (cutArea, fewerSections)
--   where (cutArea, fewerSections) = _cutCorner TORIGHT sections

calcArea :: [Section] -> Int -> Int
calcArea sections area
  | length sections == 4 = area + (meters (head sections) + 1) * (meters (head $ tail sections) + 1)
  | otherwise = calcArea fewerSections (area + cutArea)
  where (cutArea, fewerSections) = cutCorner sections

isPeninsula :: [Section] -> Bool
isPeninsula (x:y:z:xs)
  | corner x y == corner y z = True
  | otherwise = False

peninsulaArea :: [Section] ->  Int
peninsulaArea (x:y:z:xs) = min (meters x) (meters z) * (meters y + 1)

actualNegativePeninsulaArea :: [Section] -> Int
actualNegativePeninsulaArea (x:y:z:xs) = min (meters x) (meters z) * (meters y - 1)


findSmallestCut :: [Section] -> Maybe Int
findSmallestCut sections = case validIndeces of
  [] -> Nothing
  _ -> Just $ minimumBy (comparing indexArea) validIndeces
  where
    validIndeces = filter (\ i -> isPeninsula (drop i sections)) [0 .. length sections -3]
    indexArea index = peninsulaArea $ drop index sections

main :: IO ()
main = do
  input <- fmap lines getContents
  let instructions = map parseLine input
  print $ calcArea instructions 0

