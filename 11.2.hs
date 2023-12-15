import Data.Maybe (fromMaybe, listToMaybe)
import Data.List

-- safe indexing, returns '.' if out of bounds
charAt :: [String] -> Int -> Int -> Char
charAt _ (-1) _ = '.'
charAt _ _ (-1) = '.'
charAt lines row column =
  let line = fromMaybe "" $ listToMaybe $ drop row lines in
  fromMaybe '.' $ listToMaybe $ drop column line

combinations :: [a] -> [(a, a)]
combinations [] = []
combinations (x:xs) = map (\y -> (x, y)) xs ++ combinations xs

distance :: [Int] -> [Int] -> (Int, Int) -> (Int, Int) -> Int
distance emptyLines emptyColumns (l1, c1) (l2, c2) = abs (l1 - l2) + abs (c1 - c2) + 999999 * containedEmptyLines + 999999 * containedEmptyColumns
  where
    containedEmptyLines = length $ filter (\l -> l1 < l && l < l2 || l2 < l && l < l1) emptyLines
    containedEmptyColumns = length $ filter (\c -> c1 < c && c < c2 || c2 < c && c < c1) emptyColumns

main :: IO ()
main = do
  input <- fmap lines getContents
  let emptyLines = map snd $ filter (all (== '.') . fst) $ zip input [0 ..]
  let emptyColumns = map snd $ filter (all (== '.') . fst) $ zip (transpose input) [0 ..]
  let galaxies =
        [ (row, column)
          | row <- [0 .. length input - 1],
            column <- [0 .. length (input !! row) - 1],
            charAt input row column == '#'
        ]
  print $ sum $ map (uncurry (distance emptyLines emptyColumns)) $ combinations galaxies
