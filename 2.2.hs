import Data.Char (isSpace)

main :: IO()
main = do
    input <- fmap lines getContents
    print $ sum $ map (power . moves) input

-- second part of game line after colon
moves :: String -> String
moves line = unwords $ drop 2 $ words line

-- split at char
split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'

onlyRed :: String -> Int
onlyRed s = case words s of
  [x, "red"] -> read x
  _ -> 0

onlyGreen :: String -> Int
onlyGreen s = case words s of
  [x, "green"] -> read x
  _ -> 0

onlyBlue :: String -> Int
onlyBlue s = case words s of
  [x, "blue"] -> read x
  _ -> 0

-- maximum number in front of a certain color, given by onlyRed, onlyGreen, or onlyBlue
maxi :: (String -> Int) -> String -> Int
maxi f s = maximum $ map f $ concatMap (split ',') (split ';' s)

-- multiply maximums of each color
power :: String -> Int
power s = product [maxi onlyRed s, maxi onlyGreen s, maxi onlyBlue s]
