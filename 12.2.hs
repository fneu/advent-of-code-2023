import Debug.Trace
import Data.List (group, elemIndex, intercalate)
import Data.Maybe (isJust, fromJust)

-- split at char
split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'

-- doesn't impact the meaning of the pattern
removeConsecutiveDots :: String -> String
removeConsecutiveDots str = concat [if head grp == '.' then "." else grp | grp <- group str]

-- part 1
_parseLine :: String -> (String, [Int])
_parseLine line = (head w, map read $ split ',' (w!!1))
  where w = words $ removeConsecutiveDots line

-- part 2
parseLine :: String -> (String, [Int])
parseLine line = (intercalate "?" (replicate 5 p), concat $ replicate 5 l)
  where (p,l) = _parseLine line

-- checks pattern against minimum length necessary for the given numbers
plausible :: (String, [Int]) -> Bool
plausible (pattern, numbers) = length pattern >= sum numbers + length numbers - 1

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- only for pure "???" patterns
fastSolve :: (String, [Int]) -> Int
fastSolve (p, list)
  | not $ plausible (p, list) = 0
  -- factorials can be really big -> use Integer!!!
  | otherwise = fromIntegral (factorial (fromIntegral effectiveLength) `div` ( factorial (fromIntegral amount) * factorial (fromIntegral (effectiveLength - amount))))
  where effectiveLength = length p - sum list + 1
        amount = length list

-- for checking two halves of a pattern separately. Does not (recursively) evaluate second half if first half is 0
fastProduct :: (String, [Int]) -> (String, [Int]) -> Int
fastProduct (p1, l1) (p2, l2)
  | not $ plausible (p1, l1) = 0
  | not $ plausible (p2, l2) = 0
  | result1 == 0 = 0  -- it is two times faster to check the first half here instead of the second half
  | otherwise = result1 * result2
  where result2 = solve (p2, l2)
        result1 = solve (p1, l1)

-- all possible splits of a list -> [1, 2, 3] becomes [([], [1, 2, 3]), ([1], [2, 3]), ([1, 2], [3]), ([1, 2, 3], [])]
splits :: [a] -> [([a], [a])]
splits [] = [([], [])]
splits (x:xs) = ([], x:xs) : [(x:ys, zs) | (ys, zs) <- splits xs]

-- splits pattern at dot, then adds all possible combinations of numbers for each half
dotSolve :: (String, [Int]) -> Int -> Int
dotSolve (pattern, numbers) dotIndex =
  sum
    $ map
      ( \(firstSplit, secondSplit) ->
          fastProduct (take dotIndex pattern, firstSplit) (drop (dotIndex + 1) pattern, secondSplit)
      )
    $ splits numbers

-- checks if a number can be placed at a given index (of a hash) in the pattern.
-- symbols are being checked from the left, so there will not be a hash to the left of the index.
canPlace :: Int -> String -> Int -> Int -> Bool
canPlace number pattern hashIndex offset
  | hashIndex - offset < 0 = False
  | hashIndex - offset + number > length pattern = False
  | (length pattern > hashIndex - offset + number) && (pattern !! (hashIndex - offset + number) == '#') = False
  | otherwise = True

-- places any part of a number on a given hash.
-- checks left and right of the pattern with numbers before and after this one separately.
placeNumberSolve :: (String, [Int]) -> Int -> Int -> Int
placeNumberSolve (pattern, numbers) hashIndex numberIndex =
  sum $
    map
      ( \offset ->
          fastProduct
            (take (hashIndex - offset - 1) pattern, take numberIndex numbers)
            (drop (hashIndex - offset + number + 1) pattern, drop (numberIndex + 1) numbers)
      )
      (filter (canPlace number pattern hashIndex) [0 .. number - 1])
  where
    number = numbers !! numberIndex
    leftOf = take numberIndex numbers
    rightOf = drop (numberIndex + 1) numbers

-- given a hash, tries to place any part of any number on it.
hashSolve :: (String, [Int]) -> Int -> Int
hashSolve (pattern, numbers) hashIndex =
  sum $ map ( placeNumberSolve (pattern, numbers) hashIndex) [0..length numbers - 1]

-- solves any pattern
solve :: (String, [Int]) -> Int
solve (pattern, []) = if '#' `elem` pattern then  0 else 1
solve (pattern, x:xs)
  | not $ plausible (pattern, x:xs) = 0
  | head pattern == '.' = solve (tail pattern, x:xs)
  | isJust firstDotIndex = dotSolve (pattern, x:xs) (fromJust firstDotIndex)
  | isJust firstHashIndex = hashSolve (pattern, x:xs) (fromJust firstHashIndex)
  | otherwise = fastSolve (pattern, x:xs)
  where
    firstDotIndex = elemIndex '.' pattern
    firstHashIndex = elemIndex '#' pattern

main :: IO ()
main = do
  input <- fmap lines getContents
  let inputLength = show $ length input
  putStrLn $
    "result: "
      ++ show
        ( sum $
            zipWith
              ( \line index ->
                  trace
                    ("pattern " ++ show index ++ " / " ++ inputLength ++ ": " ++ show (parseLine (input !! (index - 1))))
                    (solve . parseLine)
                    line
              )
              input
              [1 ..]
        )
