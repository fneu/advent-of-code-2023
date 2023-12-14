differences :: [Int] -> [Int]
differences (x:y:xs) = (y - x) : differences (y:xs)
differences _ = []

next :: [Int] -> Int
next history = if all (==0) history then 0 else last history + next (differences history)

main :: IO ()
main = do
  input <- fmap lines getContents
  let histories = map (map (read :: String -> Int) . words) input
  print $ sum $ map next histories

