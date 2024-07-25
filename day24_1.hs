import Data.Char (isDigit, isSpace)
import Data.List (sort, tails)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe)
import Debug.Trace (trace)

data Hail = Hail
  { px :: Rational,
    py :: Rational,
    vx :: Rational,
    vy :: Rational
  }
  deriving (Show)

parseHail :: String -> Hail
parseHail s = Hail px py vx vy
  where
    ws = words (filter (/= ',') s)
    px = fromIntegral $ read $ head ws
    py = fromIntegral $ read $ ws !! 1
    vx = fromIntegral $ read $ ws !! 4
    vy = fromIntegral $ read $ ws !! 5

slope :: Hail -> Maybe Rational
slope h
  | vx h == 0 = Nothing -- vertical
  | otherwise = Just (vy h / vx h)

inFuture :: Hail -> (Rational, Rational) -> Bool
inFuture h (x, y)
  | isNothing (slope h) = undefined
  | otherwise = signum (x - px h) == signum (vx h)

intersection :: Hail -> Hail -> Maybe (Rational, Rational)
intersection a b = case intersection' a b of
  Nothing -> Nothing
  Just p ->
    if inFuture a p && inFuture b p
      then Just p
      else Nothing

intersection' :: Hail -> Hail -> Maybe (Rational, Rational)
intersection' first@(Hail px1 py1 vx1 vy1) second@(Hail px2 py2 vx2 vy2)
  | slope first == slope second = Nothing -- let's how we don't have two on the same trajectory
  | isNothing (slope first) = Just (px1, s2 * (px1 - px2) + px2)
  | isNothing (slope second) = Just (px2, s1 * (px2 - px1) + px1)
  | otherwise =
      let x = (py1 - py2 - px1 * s1 + px2 * s2) / (s2 - s1)
       in Just (x, py1 + s1 * (x - px1))
  where
    s1 = fromMaybe undefined $ slope first
    s2 = fromMaybe undefined $ slope second

inBox :: Rational -> Bool
inBox n = 200000000000000 <= n && 400000000000000 >= n

main :: IO ()
main = do
  input <- fmap lines getContents
  let hailstones = map parseHail input
  let pairs = [(x, y) | (x : xs) <- tails hailstones, y <- xs]
  let intersections = map (uncurry intersection) pairs
  let valid = filter (\(x, y) -> inBox x && inBox y) (catMaybes intersections)
  print $ length valid
