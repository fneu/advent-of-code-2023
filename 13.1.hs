import Data.List (transpose)

splitPuzzles ::  [String] -> [[String]]
splitPuzzles lines = case dropWhile (== "") lines of
  [] -> []
  lines -> firstPuzzle : splitPuzzles rest
    where (firstPuzzle, rest) = break (== []) lines

isMirror :: [String] -> [String] -> Bool
isMirror (x:xs) (y:ys) = x == y && isMirror xs ys
isMirror _ [] = True
isMirror [] _ = True

findVertical :: [String] -> [String] -> Int
findVertical reversedBefore (a:b:after)
  | (a == b) && isMirror (a:reversedBefore) (b:after) = length reversedBefore + 1
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
