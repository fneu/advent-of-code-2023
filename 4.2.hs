amount :: [String] -> Int -> Int
amount lines i =
  let n = sum $ map (\word -> if word `elem` wins then 1 else 0) picks in
  1 + sum (map (amount lines . (+) i) [1..n])
  where (wins, picks) = break (== "|") $ words (lines!!i)

main :: IO ()
main = do
  input <- fmap (map (unwords . drop 2 . words) . lines) getContents
  print $ sum $ map (amount input) [0..length input - 1]

