import Data.List (transpose)

rotateRight :: [[a]] -> [[a]]
rotateRight = transpose . reverse

rotateLeft :: [[a]] -> [[a]]
rotateLeft = reverse . transpose

rollright :: Int -> String -> String
rollright n "" = replicate n 'O'
rollright n ('.':str) = '.' : rollright n str
rollright n ('O':str) = rollright (n + 1) str
rollright n ('#':str) = replicate n 'O' ++ '#' : rollright 0 str

main :: IO ()
main = do
  input <- fmap lines getContents
  let northIsRight = rotateRight input
  let rolled = map (rollright 0) northIsRight
  let northIsDown = rotateRight rolled
  print $ sum $ zipWith (\ line index -> index * length (filter (== 'O') line)) northIsDown [1..]
