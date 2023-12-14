main :: IO ()
main = do
  input <- fmap lines getContents
  let times = map (read :: String -> Int) $ drop 1 $ words $ head input
  let distances = map (read :: String -> Int) $ drop 1 $ words $ last input
  let races = zip times distances
  print $ product $ map (length . options) races

options :: (Int, Int) -> [Int]
options (time, distance) = filter (\x -> (time - x) * x > distance) [0..time]
