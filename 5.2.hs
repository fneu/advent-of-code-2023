import Debug.Trace

main :: IO ()
main = do
  input <- fmap lines getContents

  let firstMap = parseLines (slice 4 20 input)
  let secondMap = parseLines (slice 23 36 input)
  let thirdMap = parseLines (slice 39 69 input)
  let fourthMap = parseLines (slice 72 89 input)
  let fifthMap = parseLines (slice 92 136 input)
  let sixthMap = parseLines (slice 139 176 input)
  let seventhMap = parseLines (slice 179 187 input)

  print $ minimum $ map  (alMaps seventhMap
    . alMaps sixthMap
    . alMaps fifthMap
    . alMaps fourthMap
    . alMaps thirdMap
    . alMaps secondMap
    . alMaps firstMap)
    $ concatMap range $ pairs $ fmap (read :: String -> Int) $ drop 1 $ words $ head input

parseLines :: [String] -> [(Int, Int, Int)]
parseLines = map (\line -> let [a, b, c] = map read $ words line in (a, b, c))

pairs :: [Int] -> [(Int, Int)]
pairs (x:y:cs) = (x, y) : pairs cs
pairs _ = []

range :: (Int, Int) -> [Int]
range (start,length) = [start..start + length - 1]

alMap :: (Int, Int, Int) -> Int -> Int
alMap (dest, source, range) i =
  if source <= i && (source + range) > i
    then dest + (i - source)
    else i

alMaps :: [(Int, Int, Int)] -> Int -> Int
alMaps [] i = i
alMaps (line:lines) i =
  if alMap line i == i
    then alMaps lines i
    else alMap line i

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop (from - 1) xs)
