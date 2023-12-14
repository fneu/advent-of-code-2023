import Debug.Trace

main :: IO ()
main = do
  input <- fmap lines getContents

  -- print $ minimum $ map (alMaps (slice 32 33 input)
  --   . alMaps (slice 28 29 input)
  --   . alMaps (slice 23 25 input)
  --   . alMaps (slice 19 20 input)
  --   . alMaps (slice 13 16 input)
  --   . alMaps (slice 8 10 input)
  --   . alMaps (slice 4 5 input))
  --   (fmap (read :: String -> Int) (drop 1 $ words $ head input))

  print $ minimum $ map (alMaps (slice 179 187 input)
    . alMaps (slice 139 176 input)
    . alMaps (slice 92 136 input)
    . alMaps (slice 72 89 input)
    . alMaps (slice 39 69 input)
    . alMaps (slice 23 36 input)
    . alMaps (slice 4 20 input))
    (fmap (read :: String -> Int) (drop 1 $ words $ head input))

alMap :: String -> Int -> Int
alMap line i =
  -- let (dest, source, range) = map read (words line) in
  let nums = map read (words line) in
  if nums!!1 <= i && (nums!!1 + nums!!2) > i
    then head nums + (i - nums!!1)
    else i

alMaps :: [String] -> Int -> Int
alMaps [] i = i
alMaps (line:lines) i =
  if alMap line i == i
    then alMaps lines i
    else alMap line i

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop (from - 1) xs)

