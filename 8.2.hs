import qualified Data.Map as Map

parseBranch :: String -> (String, (String, String))
parseBranch s =
  let tags = words $ filter (`notElem` "=(),") s in
    (head tags, (tags!!1, tags!!2))

walkMap :: Map.Map String (String, String) -> String -> Int -> String -> Int
walkMap map guide distance current =
  case current of
    [_, _, 'Z'] -> distance
    _ -> case Map.lookup current map of
      Nothing -> error "No such place"
      Just (a, b) -> case head guide of
        'L' -> walkMap map (tail guide) (distance + 1) a
        'R' -> walkMap map (tail guide) (distance + 1) b

main :: IO ()
main = do
  input <- fmap lines getContents
  let instructions = cycle (head input)
  let branches = map parseBranch (drop 2 input)
  let starts = filter ((== 'A') . last) $ map fst branches
  let desertMap = Map.fromList branches
  let cycles = map (walkMap desertMap instructions 0) starts
  print $ foldl lcm 1 cycles

