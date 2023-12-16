import Debug.Trace
import Data.List (nub, tails, group)

-- split at char
split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'

removeConsecutiveDots :: String -> String
removeConsecutiveDots str = concat [if head grp == '.' then "." else grp | grp <- group str]

_parseLine :: String -> (String, [Int])
_parseLine line = (head w, map read $ split ',' (w!!1))
  where w = words $ removeConsecutiveDots line

parseLine :: String -> (String, [Int])
parseLine line = trace p (p++"?"++p++"?"++p++"?"++p++"?"++p, concat $ replicate 5 l)
  where (p,l) = _parseLine line

-- factorial :: Int -> Int
-- factorial 0 = 1
-- factorial n = n * factorial (n - 1)
--
-- fastSolve :: (String, [Int]) -> Int
-- fastSolve (p, list) = 
--   let
--     effectiveLength = length p - (sum list - length list) - (length list -1)
--     amount = length list
--   in
--     if effectiveLength < amount
--       then 0
--       else factorial effectiveLength `div` ( factorial amount * factorial (effectiveLength - amount))

number :: (String, [Int]) -> Int
number (pattern, []) = if '#' `elem` pattern then  0 else 1
number (pattern, x:xs)
  | length pattern < x + sum xs + length xs = 0
  | head pattern == '.' = number (tail pattern, x:xs)
  | '.' `elem` pattern = splitSolve (pattern, x:xs)
  -- | '#' `notElem` pattern = fastSolve (pattern, x:xs) -- broken
  | otherwise =
    case head pattern of
      '#' -> if '.' `notElem` take x pattern && (length pattern == x || pattern !! x /= '#')
        then number (drop (x+(if null xs then 0 else 1)) pattern, xs)
        else 0
      '?' -> if '.' `notElem` take x pattern && (length pattern == x || pattern !! x /= '#')
        then number (drop (x+(if null xs then 0 else 1)) pattern, xs) + number (drop 1 pattern, x:xs)
        else number (drop 1 pattern, x:xs)
    where rest = number (take x pattern, xs)


allSplits :: [a] -> [([a], [a])]
allSplits [] = [([], [])]
allSplits (x:xs) = ([], x:xs) : [(x:ys, zs) | (ys, zs) <- allSplits xs]

splitSolve :: (String, [Int]) -> Int
splitSolve (pattern, numbers) =
  let splits = allSplits numbers
      (a, b) = break (== '.') pattern
   in sum $
        map
          ( \(firstSplit, secondSplit) ->
              if plausible (a, firstSplit) && plausible (b, secondSplit)
                then number (a, firstSplit) * number (b, secondSplit)
                else 0
          )
          splits
  where
    plausible (pattern, numbers) = length pattern >= sum numbers + length numbers - 1

main :: IO ()
main = do
  input <- fmap lines getContents
  print $ sum $ map (number . parseLine) input
