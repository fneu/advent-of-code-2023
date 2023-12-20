import Data.Char (isSpace, isDigit)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.List (sort, elemIndex, nub)
import Debug.Trace (trace)
import qualified Data.Map as Map

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'

data Range = Range {minr :: Int, maxr :: Int} deriving (Eq)

instance Show Range where
  show (Range min max) = show min ++ "-" ++ show max

data Part = Part
  { x :: Range,
    m :: Range,
    a :: Range,
    s :: Range
  }
  deriving (Show, Eq)

greater :: Char -> Int -> String -> Part -> (String, Part, Part)
greater c i dest p = case c of
  'x' -> (dest, Part (Range (i+1) (maxr (x p))) (m p) (a p) (s p), Part (Range (minr (x p)) i) (m p) (a p) (s p))
  'm' -> (dest, Part (x p) (Range (i+1) (maxr (m p))) (a p) (s p), Part (x p) (Range (minr (m p)) i) (a p) (s p))
  'a' -> (dest, Part (x p) (m p) (Range (i+1) (maxr (a p))) (s p), Part (x p) (m p) (Range (minr (a p)) i) (s p))
  's' -> (dest, Part (x p) (m p) (a p) (Range (i+1) (maxr (s p))), Part (x p) (m p) (a p) (Range (minr (s p)) i))

less :: Char -> Int -> String -> Part -> (String, Part, Part)
less c i dest p = case c of
  'x' -> (dest, Part (Range (minr (x p)) (i-1)) (m p) (a p) (s p), Part (Range i (maxr (x p))) (m p) (a p) (s p))
  'm' -> (dest, Part (x p) (Range (minr (m p)) (i-1)) (a p) (s p), Part (x p) (Range i (maxr (m p))) (a p) (s p))
  'a' -> (dest, Part (x p) (m p) (Range (minr (a p)) (i-1)) (s p), Part (x p) (m p) (Range i (maxr (a p))) (s p))
  's' -> (dest, Part (x p) (m p) (a p) (Range (minr (s p)) (i-1)), Part (x p) (m p) (a p) (Range i (maxr (s p))))

parseRule :: String -> Part -> (String, Part, Part)
parseRule str = case comp of
  '>' -> greater prop number dest
  '<' -> less prop number dest
  where
    prop = head str
    comp = str !! 1
    dest = split ':' str !! 1
    number = read $ filter isDigit (drop 2 str)

newtype FuncMap = FuncMap (Map.Map String (FuncMap -> Part -> [Part]))

unwrapFuncMap :: FuncMap -> Map.Map String (FuncMap -> Part -> [Part])
unwrapFuncMap (FuncMap m) = m

defaultFunc :: FuncMap -> Part -> [Part]
defaultFunc _ _ = error "No default function"

lookupFuncWithDefault :: FuncMap -> String -> (FuncMap -> Part -> [Part])
lookupFuncWithDefault funcMap str = fromMaybe defaultFunc $ Map.lookup str (unwrapFuncMap funcMap)

workflow :: String -> [Part -> (String, Part, Part)] -> FuncMap -> Part -> [Part]
workflow defaultDest [] funcMap p = case defaultDest of
  "R" -> []
  "A" -> [p]
  dest -> lookupFuncWithDefault funcMap defaultDest funcMap p
workflow defaultDest (f:fs) funcMap p = directReturns ++ workflow defaultDest fs funcMap p2
  where
      (dest, p1, p2) = f p
      directReturns = case dest of
        "R" -> []
        "A" -> [p1]
        dest -> lookupFuncWithDefault funcMap dest funcMap p1

parseWorkflow :: FuncMap -> String -> FuncMap
parseWorkflow m str = FuncMap $ Map.insert key (workflow defaultDest rules) _m
  where
    _m = unwrapFuncMap m
    key = takeWhile (/= '{') str
    ruleStrings = split ',' $ filter (`notElem` "{}") $ drop (length key) str
    rules = map parseRule (init ruleStrings)
    defaultDest = last ruleStrings

evalPart :: Part -> Integer
evalPart p = fromIntegral $ (maxr (x p) - minr (x p) + 1) * (maxr (m p) - minr (m p) + 1) * (maxr (a p) - minr (a p) + 1) * (maxr (s p) - minr (s p) + 1)

main :: IO ()
main = do
  input <- fmap lines getContents
  let splitIndex = fromMaybe 0 $ elemIndex "" input
  let rules = take splitIndex input
  let items = drop (splitIndex + 1) input
  let m = foldl parseWorkflow (FuncMap Map.empty) rules
  let part = Part (Range 1 4000) (Range 1 4000) (Range 1 4000) (Range 1 4000)
  let accepted = lookupFuncWithDefault m "in" m part
  print $ sum $ map evalPart accepted
