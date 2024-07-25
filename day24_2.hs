import Data.Char (isDigit, isSpace)
import Data.List (sort, tails)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe)
import Debug.Trace (trace)

data Hail = Hail
  { px :: Rational,
    py :: Rational,
    pz :: Rational,
    vx :: Rational,
    vy :: Rational,
    vz :: Rational
  }
  deriving (Show)

parseHail :: String -> Hail
parseHail s = Hail px py pz vx vy vz
  where
    ws = words (filter (/= ',') s)
    px = fromIntegral $ read $ head ws
    py = fromIntegral $ read $ ws !! 1
    pz = fromIntegral $ read $ ws !! 2
    vx = fromIntegral $ read $ ws !! 4
    vy = fromIntegral $ read $ ws !! 5
    vz = fromIntegral $ read $ ws !! 6

main :: IO ()
main = do
  input <- fmap lines getContents
  let hailstones = map parseHail input
  -- input specific
  -- these share common vy = 16
  let h0 = hailstones !! 81
  let h1 = hailstones !! 162
  let h2 = hailstones !! 294

  -- velocities relative to h0
  let vx1 = vx h1 - vx h0
  let vz1 = vz h1 - vz h0
  let vx2 = vx h2 - vx h0
  let vz2 = vz h2 - vz h0

  -- initial pos relative to h0
  let px1 = px h1 - px h0
  let py1 = py h1 - py h0
  let pz1 = pz h1 - pz h0
  let px2 = px h2 - px h0
  let py2 = py h2 - py h0
  let pz2 = pz h2 - pz h0

  -- we look at everything relative to h0, which stays still at it's origin
  --
  -- at some time t1, hail 1 get's struck by the rock at position x y z:
  -- x = px1 + vx1 * t1
  -- y = py1
  -- z = pz1 + vz1 * t1
  --
  -- second hail respectively at different coordinates x', y', z'.
  --
  -- rock still travels in a straight line (even when viewed relative to h0)
  -- therefore there exists a factor f: [x, y, z] = f * [x', y', z']
  --
  -- dividing both vectory by their y components ensures their equality => f is 1
  --
  -- x = (px1 + vx1 * t1) / py1
  -- y = 1
  -- z = (pz1 + vz1 * t1) / py1
  --
  -- x = (px2 + vx2 * t2) / py2
  -- y = 1
  -- z = (pz2 + vz2 * t2) / py2
  --
  -- we can get t1 and t2 from x=x and z=z:

  let t2 = ((py2 * px1 * vz1) - (vx1 * py2 * pz1) + (py1 * pz2 * vx1) - (py1 * px2 * vz1)) / (py1 * ((vz1 * vx2) - (vx1 * vz2)))
  let t1 = ((py1 * px2) + (py1 * vx2 * t2) - (py2 * px1)) / (py2 * vx1)

  -- collision points in world coordinates:

  let x1 = px h1 + (t1 * vx h1)
  let y1 = py h1 + (t1 * vy h1)
  let z1 = pz h1 + (t1 * vz h1)

  let x2 = px h2 + (t2 * vx h2)
  let y2 = py h2 + (t2 * vy h2)
  let z2 = pz h2 + (t2 * vz h2)

  -- stone velocity and initial pos

  let vx_stone = (x2 - x1) / (t2 - t1)
  let vy_stone = (y2 - y1) / (t2 - t1)
  let vz_stone = (z2 - z1) / (t2 - t1)

  let x = x1 - (vx_stone * t1)
  let y = y1 - (vy_stone * t1)
  let z = z1 - (vz_stone * t1)

  putStrLn $ "Solution: " ++ (show . truncate) (x + y + z)
