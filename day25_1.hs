import Control.DeepSeq (deepseq)
import Data.Char (isDigit, isSpace)
import Data.List (nub, sort, sortBy, tails)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Debug.Trace (trace)
import System.IO (hFlush, stdout)

type Node = String

type Graph = M.Map Node [Node]

parseLine :: String -> (Node, [Node])
parseLine l =
  let (x : xs) = words (filter (/= ':') l)
   in (x, xs)

completeGraph :: Graph -> Graph
completeGraph graph = M.foldrWithKey addReverseConnections graph graph
  where
    addReverseConnections :: Node -> [Node] -> Graph -> Graph
    addReverseConnections node connections g =
      foldr (addReverseConnection node) g connections

    addReverseConnection :: Node -> Node -> Graph -> Graph
    addReverseConnection from to g =
      let existing = fromMaybe [] (M.lookup to g)
       in M.insert to (addIfMissing from existing) g

    addIfMissing :: (Eq a) => a -> [a] -> [a]
    addIfMissing x xs
      | x `elem` xs = xs
      | otherwise = x : xs

findPath :: Graph -> Node -> [Node] -> [[Node]] -> Maybe [Node]
findPath _ _ _ [] = Nothing
findPath graph target visited (candidate : cs)
  | current == target = Just candidate
  | null children' = findPath graph target visited cs
  | otherwise = findPath graph target (current : visited) allCandidates
  where
    current = head candidate
    children = fromMaybe [] $ M.lookup current graph
    children' = filter (`notElem` visited) children
    newCandidates = map (: candidate) children'
    allCandidates = sortBy (comparing length) (cs ++ newCandidates)

tryStarts :: Graph -> Node -> [Node] -> [[Node]] -> Int
tryStarts _ _ _ [] = 0
tryStarts graph target visited starts@(s : ss) = case findPath graph target visited [s] of
  Nothing -> tryStarts graph target visited ss
  Just (n : nodes) -> 1 + tryStarts graph target (visited ++ nodes) ss

findNumberConnections :: Graph -> Node -> Node -> Int
findNumberConnections graph start end =
  tryStarts graph end [start] possibleStarts
  where
    children = fromMaybe [] $ M.lookup start graph
    possibleStarts = map (: [start]) children

sameSide :: Graph -> Node -> Node -> Bool
sameSide g n m = findNumberConnections g n m >= 4

processWithProgress :: (Node -> Bool) -> [Node] -> IO [Bool]
processWithProgress f xs = mapM processElement (zip [1 ..] xs)
  where
    total = length xs
    processElement (i, x) = do
      let result = f x
      result `deepseq` do
        putStrLn $ "Reaching node " ++ show i ++ " of " ++ show total
        hFlush stdout
        return result

main :: IO ()
main = do
  input <- fmap lines getContents
  let graph = M.fromList $ fmap parseLine input
  let graph' = completeGraph graph
  let nodes = M.keys graph'
  let start = head nodes
  let others = drop 1 nodes
  sides' <- processWithProgress (sameSide graph' start) others
  let sides = True : sides' -- initial true for starting node
  let total = length sides
  let oneSide = (length . filter id) sides
  let otherSide = total - oneSide
  let solution = oneSide * otherSide

  putStrLn $ "total nodes: " ++ show total
  putStrLn $ "one side: " ++ show oneSide
  putStrLn $ "other side: " ++ show otherSide
  putStrLn "---"
  putStrLn $ "solution: " ++ show solution
