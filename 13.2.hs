import Data.List (transpose)

splitPuzzles ::  [String] -> [[String]]
splitPuzzles lines = case dropWhile (== "") lines of
  [] -> []
  lines -> firstPuzzle : splitPuzzles rest
    where (firstPuzzle, rest) = break (== []) lines

numDifferences :: String -> String -> Int
numDifferences (x:xs) (y:ys)
  | x == y = numDifferences xs ys
  | otherwise = 1 + numDifferences xs ys
numDifferences [] [] = 0

isMirror :: Int -> [String] -> [String] -> Bool
isMirror smudges _ [] = smudges == 1
isMirror smudges [] _ = smudges == 1
isMirror smudges (x:xs) (y:ys) = (smudges + numDiffs <= 1) && isMirror (smudges + numDiffs) xs ys
  where numDiffs = numDifferences x y

findVertical :: [String] -> [String] -> Int
findVertical reversedBefore (a:b:after)
  | (numDifferences a b <= 1) && isMirror 0 (a:reversedBefore) (b:after) = length reversedBefore + 1
  | otherwise = findVertical (a:reversedBefore) (b:after)
findVertical _ [a] = 0
findVertical _ [] = 0

findHorizontal :: [String] -> Int
findHorizontal p = findVertical [] (transpose p)

evalPuzzle :: [String] -> Int
evalPuzzle p = findHorizontal p + 100 * findVertical [] p

main :: IO ()
main = do
  input <- fmap lines getContents
  let puzzles = splitPuzzles input
  print $ sum $ map evalPuzzle puzzles
