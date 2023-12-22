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

playTetris :: (M.Map Coord Int, M.Map Int [Int]) -> Block -> (M.Map Coord Int, M.Map Int [Int])
playTetris (m, restsOn) block@(Block index (Coord _ _ 1) _) = (m', restsOn)  -- block is on the ground: it neither supports nor rests on another block
  where
    m' = foldl (\m coord -> M.insert coord index m) m $ coordsOf block
playTetris (m, restsOn) block@(Block index _ _) =
  case supporters of
    [] -> playTetris (m, restsOn) (dropOne block) -- if no supporters: this block can fall
    _ -> (m', restsOn') -- block is supported by at least one other block: place block and update maps
  where
    supporters = nub $ mapMaybe (`M.lookup` m) $ coordsBelow block
    m' = foldl (\m coord -> M.insert coord index m) m $ coordsOf block
    restsOn' = foldl (\r' supporter -> M.insertWith (++) index [supporter] r') restsOn supporters

countFalling :: M.Map Int [Int] -> [Int] -> [Block] -> Int
countFalling restsOn falling [] = length falling - 1  -- initial falling block doesn't count
countFalling restsOn falling (block:blocks) = -- should be find to iterate over blocks here because they are sorted. thus, we're checking from the bottom up.
  case directSupports of
    [] -> countFalling restsOn falling blocks -- block rests on ground and cannot fall
    _ -> case filter (`notElem` falling) directSupports of
      [] -> countFalling restsOn (index block:falling) blocks -- all of this blocks supporters are falling and it will fall too
      _ -> countFalling restsOn falling blocks  -- block is supported by at least one block that is not falling: block cannot fall
  where
    directSupports = M.findWithDefault [] (index block) restsOn

main :: IO()
main = do
  input <- fmap lines getContents
  let blocks = sort $ zipWith parseBlock [0..] input
  let (mCoords, mRestsOn) = foldl playTetris (M.empty, M.empty) blocks
  let falling = map (\i -> countFalling mRestsOn [i] blocks) [0..length blocks - 1]
  print $ sum falling
