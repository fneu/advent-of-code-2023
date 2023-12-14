main = do
  input <- fmap lines getContents
  let time = (read :: String -> Int) $ concat $ drop 1 $ words $ head input
  let distance = (read :: String -> Int) $ concat $ drop 1 $ words $ last input
  print $ length $ filter (\x -> (time - x) * x > distance) [0..time]
