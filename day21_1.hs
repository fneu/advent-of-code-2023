import Data.Char (isSpace, isDigit)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.List (sort, elemIndex, nub)
import Debug.Trace (trace)
import qualified Data.Map as M

children :: [(Int, Int)] -> [(Int, Int)]
children [] = []
children list = nub $ concatMap (\(x,y) -> [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]) list

takeAndGrow :: ([(Int, Int)], [(Int, Int)], [(Int, Int)]) -> ([(Int, Int)], [(Int, Int)], [(Int, Int)])
takeAndGrow (candidates, reachable, current) = (candidates', reachable', current')
  where
    current' = filter (`elem` candidates) $ children current
    candidates' = filter (`notElem` current') candidates
    reachable' = reachable ++ current

abandonAndGrow :: ([(Int, Int)], [(Int, Int)], [(Int, Int)]) -> ([(Int, Int)], [(Int, Int)], [(Int, Int)])
abandonAndGrow (candidates, reachable, current) = (candidates', reachable, current')
  where
    current' = filter (`elem` candidates) $ children current
    candidates' = filter (`notElem` current') candidates

takeNSteps :: Int -> Bool -> ([(Int, Int)], [(Int, Int)], [(Int, Int)]) -> [(Int, Int)]
takeNSteps 0 True (candidates, reachable, current) = reachable ++ current
takeNSteps 0 False (candidates, reachable, current) = reachable
takeNSteps n True (candidates, reachable, current) = takeNSteps (n-1) False $ takeAndGrow (candidates, reachable, current)
takeNSteps n False (candidates, reachable, current) = takeNSteps (n-1) True $ abandonAndGrow (candidates, reachable, current)

main :: IO ()
main = do
  input <- fmap lines getContents
  let positions = [((row, col),input !! row !! col) | row <- [0.. length input - 1], col <- [0..length (head input) - 1]]
  let start = fst $ head $ filter (\(_,c) -> c == 'S') positions
  let available = map fst $ filter (\(_,c) -> c == '.') positions
  let reachable = takeNSteps 64 True (available, [], [start])
  print $ length reachable

