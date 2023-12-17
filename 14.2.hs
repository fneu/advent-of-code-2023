import Data.List (transpose)
import Debug.Trace (trace)

rotateRight :: [[a]] -> [[a]]
rotateRight = transpose . reverse

rollright :: Int -> String -> String
rollright n "" = replicate n 'O'
rollright n ('.':str) = '.' : rollright n str
rollright n ('O':str) = rollright (n + 1) str
rollright n ('#':str) = replicate n 'O' ++ '#' : rollright 0 str

spinCycles :: Int -> [String] -> [String]
spinCycles 0 puzzle = puzzle
spinCycles n puzzle = trace ("n = " ++ show n ++ "; load = " ++ show total_load) puzzle
  where
    puzzle = spinCycles (n - 1) $ ( map (rollright 0) . rotateRight . map(rollright 0) . rotateRight . map (rollright 0) . rotateRight . map (rollright 0) . rotateRight) puzzle
    total_load = sum $ zipWith (\ line index -> index * length (filter (== 'O') line)) ((rotateRight . rotateRight) puzzle) [1..]

main :: IO ()
main = do
  input <- fmap lines getContents
  let spinned = spinCycles 1000 input
  let northIsDown = (rotateRight . rotateRight) spinned
  print $ sum $ zipWith (\ line index -> index * length (filter (== 'O') line)) northIsDown [1..]
  -- 89044 is low
  -- 89049 is low
