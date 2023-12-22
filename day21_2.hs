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

-- Mostly a paperback solution. Geometric explanation:
--
--     ┌───┬─A─┬───┐
--     │   │/ \│   │
--     │ S'/ O'\ S'│
--     │  /│   │\  │
-- ┌───┼─/─┼───┼─\─┼───┐
-- │   │/  │   │  \│   │
-- │ S'/ O'│ S │ O'\ S'│
-- │  /│   │   │   │\  │
-- ├─/─┼───┼───┼───┼─\─┤
-- │/  │   │   │   │  \│
-- < O'│ S │ O │ S │ O'>
-- │\  │   │   │   │  /│
-- ├─\─┼───┼───┼───┼─/─┤
-- │  \│   │   │   │/  │
-- │ S'\ O'│ S │ O'/ S'│
-- │   │\  │   │  /│   │
-- └───┼─\─┼───┼─/─┼───┘
--     │  \│   │/  │
--     │ S'\ O'/ S'│
--     │   │\ /│   │
--     └───┴─V─┴───┘
--
-- Notice that the vertical and horizontal center lines of the input are empty.
-- Necessary steps are 26501365 = which takes us to the outside of the initial map (65 steps)
-- and then let's us walk across n=202300 full maps.
-- (26501365 - 65) / 131 = n = 202300.
--
-- The reachable area grows in a diamond shape shown above for n = 2.
-- In the center maps, every other cell can be reached.
-- There is two versions: Those where the center is reached are marked with S,
-- the others are marked with O **and** O'.
-- There is n*n of the first kind and (n+1)*(n+1) of the second kind.
--
-- Additionally, there are 4*n maps marked S' where only a corner is reached.
-- These add up to n 'outer S diamonds' containing only those garden plots of a map
-- that cannot be reached from the center in 65 steps.
-- 
-- ┌─A─┐
-- │/ \│
-- < S >
-- │\ /│
-- └─V─┘
-- 
-- Lastly, the maps marked O' are missing one or two corners, adding up to (n+1)
-- 'outer O diamonds' that need to be subtracted from the result.

main :: IO ()
main = do
  input <- fmap lines getContents
  let positions = [((row, col),input !! row !! col) | row <- [0.. length input - 1], col <- [0..length (head input) - 1]]
  let start = fst $ head $ filter (\(_,c) -> c == 'S') positions
  let available = map fst $ filter (\(_,c) -> c == '.') positions
  let fullS = length $ takeNSteps 131 True (available, [], [start])
  let fullO = length $ takeNSteps 131 False (available, [], [start])
  let innerSDiamond = length $ takeNSteps 65 True (available, [], [start])
  let innerODiamond = length $ takeNSteps 65 False (available, [], [start])
  let outerSDiamond = fullS - innerSDiamond
  let outerODiamond = fullO - innerODiamond
  let n = 202300
  print $ 
    n * n * fullS 
    + (n+1) * (n+1) * fullO 
    + n * outerSDiamond
    - (n+1) * outerODiamond
