import Data.List (transpose, elemIndex)
import Debug.Trace (trace)

rotateRight :: [[a]] -> [[a]]
rotateRight = transpose . reverse

rollright :: Int -> String -> String
rollright n "" = replicate n 'O'
rollright n ('.':str) = '.' : rollright n str
rollright n ('O':str) = rollright (n + 1) str
rollright n ('#':str) = replicate n 'O' ++ '#' : rollright 0 str

spinCycle :: [String] -> [String]
spinCycle = map (rollright 0) . rotateRight . map (rollright 0) . rotateRight . map (rollright 0) . rotateRight . map (rollright 0) . rotateRight

findRepetition :: [[String]] -> [String] -> ([[String]], Int)
findRepetition states state = case elemIndex state states of
  Just index -> (states, index)
  Nothing -> findRepetition (state:states) (spinCycle state)

main :: IO ()
main = do
  input <- fmap lines getContents
  let (states, index) = findRepetition [] input
  let beforeRepetition = length states - index - 1
  let lengthRepetition = index + 1
  let offset = (1000000000 - beforeRepetition) `mod` lengthRepetition
  let equiv = lengthRepetition - offset - 1
  let evalPattern = rotateRight $ rotateRight $ states!!equiv
  print $ sum $ zipWith (\ line index -> index * length (filter (== 'O') line)) evalPattern [1..]
