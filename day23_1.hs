import Data.Char (isSpace, isDigit)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.List (sort)
import Debug.Trace (trace)
import qualified Data.Map as M

data Position = Position{
  row :: Int,
  column :: Int
} deriving (Show, Eq, Ord)

data Step = Step{
  position :: Position,
  history :: [Position]
}

candidates :: M.Map Position Char -> Step -> [Step]
candidates map s@(Step p@(Position row column) history) = filter (\child -> position child `notElem` history) possible
  where
    neighbors =
      case M.lookup p map of
        Just '#' -> error "Get off the wall!"
        Just '.' -> [Step (Position (row - 1) column) (p:history),
                     Step (Position (row + 1) column) (p:history),
                     Step (Position row (column - 1)) (p:history),
                     Step (Position row (column + 1)) (p:history)]
        Just '^' -> [Step (Position (row - 1) column) (p:history)]
        Just 'v' -> [Step (Position (row + 1) column) (p:history)]
        Just '<' -> [Step (Position row (column - 1)) (p:history)]
        Just '>' -> [Step (Position row (column + 1)) (p:history)]
        Nothing -> []  -- We're out of the map
    possible = filter (\n -> M.lookup (position n) map /= Just '#') neighbors

maxDist :: M.Map Position Char -> Step -> Int
maxDist m s = case candidates m s of
  [] -> length ( history s ) - 1
  cs -> maximum $ map (maxDist m) cs

main :: IO ()
main = do
  input <- fmap lines getContents
  let map = M.fromList $ concat $ zipWith (\row line -> zipWith (\column char -> (Position row column, char)) [0..] line) [0..] input
  print $ maxDist map $ Step (Position 0 1) []
