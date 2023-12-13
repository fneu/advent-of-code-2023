{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
import Data.Char (isSpace, isDigit)
import Data.Maybe (fromMaybe, listToMaybe)
import Debug.Trace

-- safe indexing, returns '.' if out of bounds
charAt :: [String] -> Int -> Int -> Char
charAt _ (-1) _ = '.'
charAt _ _ (-1) = '.'
charAt lines row column =
  let line = fromMaybe "" $ listToMaybe $ drop row lines in
  fromMaybe '.' $ listToMaybe $ drop column line

-- touches exactly two numbers, result is product
gearNumber :: [String] -> Int -> Int -> Int
gearNumber lines row column =
  if charAt lines row column /= '*' then 0 else
  let
     left = if isDigit (charAt lines row (column-1)) then [numberAt lines row (column-1)] else []
     right = if isDigit (charAt lines row (column+1)) then [numberAt lines row (column+1)] else []
     up = case (isDigit (charAt lines (row-1) (column-1)), isDigit (charAt lines (row-1) column), isDigit (charAt lines (row-1) (column+1))) of
       (True, True, _) -> [numberAt lines (row-1) (column-1)]
       (True, False, True) -> [numberAt lines (row-1) (column-1), numberAt lines (row-1) (column+1)]
       (True, False, False) -> [numberAt lines (row-1) (column-1)]
       (False, False, False) -> []
       (False, False, True) -> [numberAt lines (row-1) (column+1)]
       (False, True, _) -> [numberAt lines (row-1) column]
     down = case (isDigit (charAt lines (row+1) (column-1)), isDigit (charAt lines (row+1) column), isDigit (charAt lines (row+1) (column+1))) of
       (True, True, _) -> [numberAt lines (row+1) (column-1)]
       (True, False, True) -> [numberAt lines (row+1) (column-1), numberAt lines (row+1) (column+1)]
       (True, False, False) -> [numberAt lines (row+1) (column-1)]
       (False, False, False) -> []
       (False, False, True) -> [numberAt lines (row+1) (column+1)]
       (False, True, _) -> [numberAt lines (row+1) column]
     in
     if length (left ++ right ++ up ++ down) /= 2 then 0 else
      product (left ++ right ++ up ++ down)

-- needs to start at a digit, any digit of the start number
numberAt :: [String] -> Int -> Int -> Int
numberAt lines row column =
  if isDigit (charAt lines row (column-1)) then numberAt lines row (column-1) else readBeginningAt lines row column []

-- starts at the leftmost digit of a number
readBeginningAt :: [String] -> Int -> Int -> [Char] -> Int
readBeginningAt lines row column carryOver =
  if isDigit (charAt lines row column)
    then readBeginningAt lines row (column+1) (carryOver ++ [charAt lines row column])
    else read carryOver

main :: IO()
main = do
    input <- fmap lines getContents
    print $ sum $ map (\line -> sum (map (gearNumber input line) [0..length (head input) - 1])) [0..length input - 1]


