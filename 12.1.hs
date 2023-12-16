import Debug.Trace
import Data.List (nub)

-- split at char
split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'

parseLine :: String -> (String, [Int])
parseLine line = (head w, map read $ split ',' (w!!1))
  where w = words line

minimumPattern :: [Int] -> String -> String
minimumPattern [] s = s
minimumPattern (i:xs) "" = minimumPattern xs $ replicate i '#'
minimumPattern (i:xs) s = minimumPattern xs $ s ++ "." ++ replicate i '#'

extensions :: [String] -> String -> String -> [String]
extensions options start "" = nub $ (start ++ "."):options
extensions options "" (x:xs) = extensions ["." ++ x:xs] [x] xs
extensions options start (x:xs) = case x of
  '.' -> extensions ((start ++ "." ++ x:xs):options) (start ++ ".") xs
  '#' -> extensions options (start ++ "#") xs

extendN :: Int -> [String] -> [String]
extendN 0 options = options
extendN n options = extendN (n-1) $ nub $ concatMap (extensions [] "") options

check :: String -> String -> Bool
check "" "" = True
check (a:as) (b:bs) = case a of
  '?' -> check as bs
  '.' -> (b == '.') && check as bs
  '#' -> (b == '#') && check as bs
  _ -> error $ "FUCK! why am I comparing" ++ a:as ++ " and " ++ b:bs ++ "?"

numberOfOptions ::(String, [Int]) -> Int
numberOfOptions (damagedPattern, numbers) =
  let
    minPattern = minimumPattern numbers ""
    lenDiff = length damagedPattern - length minPattern
  in
    length $ filter (check damagedPattern) $ extendN lenDiff [minPattern]

main :: IO ()
main = do
  input <- fmap lines getContents
  print $ sum $ map (numberOfOptions . parseLine) input
