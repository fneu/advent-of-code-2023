import Debug.Trace
import Data.List (nub)
import Data.Char

-- split at char
split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'


_hash :: String -> Int
_hash "" = 0
_hash (x:xs) = (17 * (ord x + currentValue)) `mod` 256
  where currentValue = _hash xs

hash :: String -> Int
hash = _hash . reverse

main :: IO ()
main = do
  input <- getContents
  let noNewLines = filter (/= '\n') input
  print $ sum $ map hash $ split ',' noNewLines
