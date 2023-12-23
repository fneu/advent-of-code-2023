import Data.Char (isSpace, isDigit)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes, isNothing, mapMaybe)
import Data.List (sort, maximumBy)
import Debug.Trace (trace)
import qualified Data.Map as M

data Position = Position{
  row :: Int,
  column :: Int
} deriving (Eq, Ord)

instance Show Position where
  show (Position row column) = "(" ++ show row ++ ", " ++ show column ++ ")"

data Step = Step{
  position :: Position,
  history :: [Position]
}

-- Jup, code quality is shit for this day, but it works ...

data Step2 = Step2{
  position2 :: Position,
  history2 :: [Position],
  pathLength :: Int
}

data Connection = Connection {
  to :: Position,
  distance :: Int
} deriving (Show)

data Node = Node{
  nodePos :: Position,
  neighbors :: [Connection]
} deriving (Show)

isNode :: M.Map Position Char -> Position -> Bool
isNode map p@(Position row column) = length possible > 2
  where
    neighbors =
      case M.lookup p map of
        Just '#' -> []
        Nothing -> []  -- We're out of the map
        _ -> [Position (row - 1) column,
              Position (row + 1) column,
              Position row (column - 1),
              Position row (column + 1)]
    possible = filter (\n -> M.lookup n map /= Just '#') neighbors


candidates :: M.Map Position Char -> Step -> [Step]
candidates map s@(Step p@(Position row column) history) = filter (\child -> position child `notElem` history) possible
  where
    neighbors =
      case M.lookup p map of
        Just '#' -> error "Get off the wall!"
        Nothing -> []  -- We're out of the map
        _ -> [Step (Position (row - 1) column) (p:history),
              Step (Position (row + 1) column) (p:history),
              Step (Position row (column - 1)) (p:history),
              Step (Position row (column + 1)) (p:history)]
    possible = filter (\n -> M.lookup (position n) map /= Just '#') neighbors

findConnection :: M.Map Position Char -> Node -> Step -> Maybe Connection
findConnection m finish s = case candidates m s of
  [] -> Nothing
  [c] -> if position c == nodePos finish then Just $ Connection (position c) (length (history s)+1) else findConnection m finish c
  cs -> Just $ Connection (position s) (length $ history s)

makeNode :: M.Map Position Char -> Node -> Position -> Node
makeNode m finish p = Node p $ mapMaybe (findConnection m finish) (candidates m $ Step p [])

maxDist :: M.Map Position Node -> Node -> Step2 -> Maybe Int
maxDist m finish s = case availableConnections of
  [] -> if position2 s == nodePos finish then Just (pathLength s) else Nothing
  cs -> maximum $ map (maxDist m finish . (\c -> Step2 (to c) (position2 s:history2 s) (pathLength s + distance c))) cs
  where
    node = case M.lookup (position2 s) m of
      Just n -> n
      Nothing -> error "not a node"
    availableConnections = filter (\c -> to c `notElem` history2 s) (neighbors node)

main :: IO ()
main = do
  input <- fmap lines getContents
  let m = M.fromList $ concat $ zipWith (\row line -> zipWith (\column char -> (Position row column, char)) [0..] line) [0..] input
  let nodePoses = Position 0 1:Position (length input - 1) (length (head input) - 2):filter (isNode m) (M.keys m)
  let nodes = map (makeNode m (Node (nodePoses!!1) [])) nodePoses
  let nodeMap = M.fromList $ map (\n -> (nodePos n, n)) nodes
  print $ maxDist nodeMap (nodes!!1) $ Step2 (Position 0 1) [] 0
