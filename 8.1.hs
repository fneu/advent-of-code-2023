import qualified Data.Map as Map

parseBranch :: String -> (String, (String, String))
parseBranch s =
  let tags = words $ filter (`notElem` "=(),") s in
    (head tags, (tags!!1, tags!!2))

walkMap :: Map.Map String (String, String) -> String -> String -> Int -> Int
walkMap map guide current distance =
  case current of
    "ZZZ" -> distance
    _ -> case Map.lookup current map of
      Nothing -> error "No such place"
      Just (a, b) -> case head guide of
        'L' -> walkMap map (tail guide) a (distance + 1)
        'R' -> walkMap map (tail guide) b (distance + 1)

main :: IO ()
main = do
  input <- fmap lines getContents
  let instructions = cycle (head input)
  let branches = map parseBranch (drop 2 input)
  let desertMap = Map.fromList branches
  print $ walkMap desertMap instructions "AAA" 0
