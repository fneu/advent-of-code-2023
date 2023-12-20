import Data.Char (isSpace, isDigit)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.List (sort, elemIndex)
import Debug.Trace (trace)
import qualified Data.Map as Map

data Part = Part
  { x :: Int,
    m :: Int,
    a :: Int,
    s :: Int
  }
  deriving (Show, Eq)

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'

parsePart :: String -> Part
parsePart str = case map read $ split ',' s of
  [x, m, a, s] -> Part x m a s
  where s = filter (`elem` "0123456789,") str

data Result = Accepted | Rejected deriving (Show, Eq)

newtype FuncMap = FuncMap (Map.Map String (FuncMap -> Part -> Result))

unwrapFuncMap :: FuncMap -> Map.Map String (FuncMap -> Part -> Result)
unwrapFuncMap (FuncMap m) = m

lookupFunc :: FuncMap -> String -> Maybe (FuncMap -> Part -> Result)
lookupFunc funcMap str = Map.lookup str (unwrapFuncMap funcMap)

defaultFunc :: FuncMap -> Part -> Result
defaultFunc _ _ = Rejected

lookupFuncWithDefault :: FuncMap -> String -> (FuncMap -> Part -> Result)
lookupFuncWithDefault funcMap str = fromMaybe defaultFunc $ Map.lookup str (unwrapFuncMap funcMap)

paramFunc :: (Part -> Int) -> (Int -> Bool) -> String -> Part -> Maybe String
paramFunc prop comp str part = if comp (prop part) then Just str else Nothing

aFunc :: Part -> Maybe String
aFunc _ = Just "A"

rFunc :: Part -> Maybe String
rFunc _ = Just "R"

boringFunc :: String -> Part -> Maybe String
boringFunc str _ = Just str

combinedFunc :: [Part -> Maybe String] -> FuncMap -> Part -> Result
combinedFunc (f:funcs) m p = case f p of
  Just str -> case str of
    "A" -> Accepted
    "R" -> Rejected
    _ -> lookupFuncWithDefault m str m p
  Nothing -> combinedFunc funcs m p

parseRule :: String -> (Part -> Maybe String)
parseRule str
  | str == "A" = aFunc
  | str == "R" = rFunc
  | '<' `notElem` str && '>' `notElem` str = boringFunc str
  | otherwise = paramFunc prop comp res
  where
    prop = case head str of
      'x' -> x
      'm' -> m
      'a' -> a
      's' -> s
    comp = case str !! 1 of
      '<' -> (< (read $ takeWhile isDigit $ drop 2 str))
      '>' -> (> (read $ takeWhile isDigit $ drop 2 str))
      _ -> error ("unknown comparison" ++ str)
    res = split ':' str !! 1

parseFunc :: FuncMap -> String -> FuncMap
parseFunc m str = FuncMap $ Map.insert key (combinedFunc rules) _m
  where
    _m = unwrapFuncMap m
    key = takeWhile (/= '{') str
    ruleStrings = split ',' $ filter (`notElem` "{}") $ drop (length key) str
    rules = map parseRule ruleStrings

evalPart :: Part -> Int
evalPart p = x p + m p + a p + s p

main :: IO ()
main = do
  input <- fmap lines getContents
  let splitIndex = fromMaybe 0 $ elemIndex "" input
  let rules = take splitIndex input
  let items = drop (splitIndex + 1) input
  let m = foldl parseFunc (FuncMap Map.empty) rules
  let parts = map parsePart items
  let accepted = filter (\p -> lookupFuncWithDefault m "in" m p == Accepted) parts
  print $ sum $ map evalPart accepted



