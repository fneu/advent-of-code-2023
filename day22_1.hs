import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.List (sort, nub)

data Coord = Coord { x :: Int, y :: Int, z :: Int } deriving (Eq, Ord)

data Block = Block {
  index :: Int,
  min :: Coord,
  max :: Coord
  } deriving (Eq)

instance Ord Block where
  compare (Block _ (Coord _ _ z1) _ ) (Block _ (Coord _ _ z2) _ ) = compare z1 z2

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'

parseCoord :: String -> Coord
parseCoord str = Coord x y z
  where
    [x, y, z] = map read $ split ',' str

parseBlock index str = Block index min max
  where
    (_min:_max:_) = split '~' str
    min = parseCoord _min
    max = parseCoord _max

coordsOf :: Block -> [Coord]
coordsOf (Block _ (Coord x1 y1 z1) (Coord x2 y2 z2)) = [Coord x y z | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]]

dropOne :: Block -> Block
dropOne (Block index (Coord x1 y1 z1) (Coord x2 y2 z2)) = Block index (Coord x1 y1 (z1-1)) (Coord x2 y2 (z2-1))

coordsBelow :: Block -> [Coord]
coordsBelow block = filter (`notElem` coordsOf block) $ coordsOf $ dropOne block

playTetris :: (M.Map Coord Int, [Int]) -> Block -> (M.Map Coord Int, [Int])
playTetris (m, singleSupporters) block@(Block index (Coord _ _ 1) _) = (m', singleSupporters)
  where
    m' = foldl (\m coord -> M.insert coord index m) m $ coordsOf block  -- block is on the ground: place it and do not add to blacklist
playTetris (m, singleSupporters) block@(Block index _ _) =
  case supporters of
    [] -> playTetris (m, singleSupporters) (dropOne block) -- if no supporters: this block can fall
    [supporter] -> (m', supporter:singleSupporters) -- if single supporter: place block and add single supporter to blacklist: cannot be removed
    _ -> (m', singleSupporters) -- if multiple supporters: place block and do not add to blacklist
  where
    supporters = nub $ mapMaybe (`M.lookup` m) $ coordsBelow block
    m' = foldl (\m coord -> M.insert coord index m) m $ coordsOf block

main :: IO()
main = do
  input <- fmap lines getContents
  let blocks = sort $ zipWith parseBlock [0..] input
  let (_, singleSupporters) = foldl playTetris (M.empty, []) blocks
  let safeToRemove = filter (\(Block index _ _) -> index `notElem` singleSupporters) blocks
  print $ length safeToRemove
