import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust)
import Data.List (elemIndex)
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

data Action = Add | Remove
  deriving (Show)

data Instruction = Instruction
  { action :: Action,
    label :: String,
    focalLength :: Maybe Int,
    box :: Int
  }
  deriving (Show)

data Lens = Lens {
  lensLabel :: String,
  lensFocalLength :: Int
  }
  deriving (Show)

parse :: String -> Instruction
parse str =
  case '=' `elemIndex` str of
    Just index ->
      Instruction
        { action = Add,
          label = take index str,
          focalLength = Just $ read $ drop (index + 1) str,
          box = hash $ take index str
        }
    Nothing ->
      Instruction
        { action = Remove,
          label = take (fromJust (elemIndex '-' str)) str,
          focalLength = Nothing,
          box = hash $ take (fromJust (elemIndex '-' str)) str
        }

apply :: [Lens] -> Instruction -> [Lens]
apply [] instruction = case action instruction of
  Add -> [Lens (label instruction) (fromMaybe 0 $ focalLength instruction)]
  Remove -> []
apply (lens : lenses) instruction = case action instruction of
  Add ->
    ( if lensLabel lens == label instruction
        then Lens (label instruction) (fromMaybe 0 $ focalLength instruction) : lenses
        else lens : apply lenses instruction
    )
  Remove -> filter (\l -> lensLabel l /= label instruction) (lens:lenses)

boxes :: Map.Map Int [Lens] -> [Instruction] -> Map.Map Int [Lens]
boxes m [] = m
boxes m (x:xs) = boxes (Map.insert (box x) newLenses m) xs
    where
      newLenses =  apply (fromMaybe [] $ Map.lookup (box x) m) x

power :: [Lens] -> Int
power [] = 0
power lenses = sum $ zipWith (*) [1..] $ map lensFocalLength lenses

mapPower :: Map.Map Int [Lens] -> Int
mapPower m = sum $ zipWith (*) [1..] $ map (power . (\index -> fromMaybe [] $ Map.lookup index m)) [0..255]

main :: IO ()
main = do
  input <- getContents
  let noNewLines = filter (/= '\n') input
  let instructions = map parse $ split ',' noNewLines
  print $ mapPower $ boxes Map.empty instructions
