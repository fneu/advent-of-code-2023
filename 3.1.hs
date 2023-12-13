import Data.Char (isSpace, isDigit)
import Data.Maybe (fromMaybe, listToMaybe)

-- safe indexing, returns '.' if out of bounds
charAt :: [String] -> Int -> Int -> Char
charAt _ (-1) _ = '.'
charAt _ _ (-1) = '.'
charAt lines row column =
  let line = fromMaybe "" $ listToMaybe $ drop row lines in
  fromMaybe '.' $ listToMaybe $ drop column line

isSymbol :: Char -> Bool
isSymbol = flip notElem "0123456789."

-- adjacent digits make their entire number a part number
isAdjacent :: [String] -> Int -> Int -> Bool
isAdjacent lines row column =
  isSymbol (charAt lines (row-1) (column-1))
  || isSymbol (charAt lines (row-1) column)
  || isSymbol (charAt lines (row-1) (column+1))
  || isSymbol (charAt lines row (column-1))
  || isSymbol (charAt lines row (column+1))
  || isSymbol (charAt lines (row+1) (column-1))
  || isSymbol (charAt lines (row+1) column)
  || isSymbol (charAt lines (row+1) (column+1))

numberAt :: [String] -> Int -> Int -> Int
numberAt lines row column =
  case (isDigit (charAt lines row (column-1)), isDigit (charAt lines row column)) of
    (False, True) -> readBeginningAt lines row column [] False
    _ -> 0

-- we're starting at a digit, let's start to read a number that might be qualified
readBeginningAt :: [String] -> Int -> Int -> [Char] -> Bool -> Int
readBeginningAt lines row column carryOver alreadyQualified =
  case (
    alreadyQualified,
    isAdjacent lines row column,
    isDigit (charAt lines row column)
  ) of
    (True, _, True) -> readBeginningAt lines row (column+1) (carryOver ++ [charAt lines row column]) True
    (False, True, True) -> readBeginningAt lines row (column+1) (carryOver ++ [charAt lines row column]) True
    (False, False, True) -> readBeginningAt lines row (column+1) (carryOver ++ [charAt lines row column]) False
    (True, _, False) -> read carryOver
    (False, _, False) -> 0

main :: IO()
main = do
    input <- fmap lines getContents
    print $ map (numberAt input 0) [0..length (head input) - 1]
    print $ sum $ map (\line -> sum (map (numberAt input line) [0..length (head input) - 1])) [0..length input - 1]

