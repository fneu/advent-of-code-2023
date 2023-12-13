import Data.Char (isSpace)

main :: IO()
main = do
    input <- fmap lines getContents
    -- print $ split ';' $ moves $ head input
    print $ sum $ map gameNumber $ filter (checkLine . moves) input

gameNumber :: String -> Int
gameNumber line = read $ init $ words line !! 1

-- second part of game line after colon
moves :: String -> String
moves line = unwords $ drop 2 $ words line

-- split at char
split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'

-- check if move like " 7 blue " satisfies amount in problem
checkAmount :: String -> Bool
checkAmount s = case words s of
  [x, "red"] -> read x <= 12
  [x, "green"] -> read x <= 13
  [x, "blue"] -> read x <= 14
  _ -> False

checkLine :: String -> Bool
checkLine line = all (all checkAmount . split ',') ( split ';' line)
