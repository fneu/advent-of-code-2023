main :: IO ()
main = do
  input <- fmap (map (unwords . drop 2 . words) . lines) getContents
  print $ sum $ map winnings input

winnings :: String -> Int
winnings line =
  let n = sum $ map (\word -> if word `elem` wins then 1 else 0) picks in
  case n of
    0 -> 0
    _ -> 2 ^ (n - 1)
  where (wins, picks) = break (== "|") $ words line
