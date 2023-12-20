import Data.Char (isSpace, isDigit)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.List (sort, elemIndex)
import Debug.Trace (trace)
import qualified Data.Map as Map

data Pulse = Low | High deriving (Show, Eq)

data FlipFlopState = On | Off deriving (Show, Eq)

flipTheFlop :: FlipFlopState -> FlipFlopState
flipTheFlop On = Off
flipTheFlop Off = On

newtype ConjunctionState = ConjunctionState (Map.Map String Pulse) deriving (Show, Eq)

data ModuleState = FlipFlop FlipFlopState | Conjunction ConjunctionState | Default deriving (Show, Eq)

data ModuleType = FlipFlopType | ConjunctionType | BroadcastType deriving (Show, Eq)

data Module = Module {
  moduleType :: ModuleType,
  moduleState :: ModuleState,
  recipients :: [String]
} deriving (Show, Eq)

data Signal = Signal {
  origin :: String,
  address :: String,
  pulse :: Pulse
} deriving (Eq)

instance Show Signal where
  show (Signal o a High) = o ++ " -high-> " ++ a
  show (Signal o a Low) = o ++ " -low-> " ++ a

parseModule :: String -> (String, Module)
parseModule s = (name, _module)
  where
    moduleType = case head s of
      '%' -> FlipFlopType
      '&' -> ConjunctionType
      'b' -> BroadcastType
    name = case tail $ head $ words s of
      "roadcaster" -> "broadcaster"
      n -> n
    recipients = drop 2 $ words $ filter (/= ',') s
    moduleState = case moduleType of
      FlipFlopType -> FlipFlop Off
      ConjunctionType -> Conjunction $ ConjunctionState Map.empty -- TODO: update with list of origins
      BroadcastType -> Default
    _module = Module moduleType moduleState recipients

handleSignal :: Map.Map String Module -> [Signal] -> [Signal] -> (Map.Map String Module,[Signal])
handleSignal m handled [] = (m, handled)
handleSignal m handled (s@(Signal o "output" p):ss) = handleSignal m (handled ++ [s]) ss
handleSignal m handled (s@(Signal o "rx" p):ss) = handleSignal m (handled ++ [s]) ss
handleSignal m handled (s:ss) = handleSignal m' (handled ++ [s]) (ss ++ newSignals)
  where
    oldModuleState = case Map.lookup (address s) m of
      Just state -> state
      Nothing -> error $ "Module not found: " ++ address s
    newModuleState = case moduleType oldModuleState of
      FlipFlopType -> case pulse s of
        High -> oldModuleState
        Low -> oldModuleState { moduleState = case moduleState oldModuleState of
          FlipFlop On -> FlipFlop Off
          FlipFlop Off -> FlipFlop On
        }
      ConjunctionType -> oldModuleState { moduleState = Conjunction $ ConjunctionState $ Map.insert (origin s) (pulse s) $ case moduleState oldModuleState of
        Conjunction (ConjunctionState _m) -> _m
      }
      _ -> oldModuleState
    newSignals = case moduleType newModuleState of
      FlipFlopType -> case pulse s of
        High -> []
        Low -> case moduleState newModuleState of
          FlipFlop Off -> [Signal (address s) recipient Low | recipient <- recipients newModuleState]
          FlipFlop On -> [Signal (address s) recipient High | recipient <- recipients newModuleState]
      BroadcastType -> [Signal (address s) recipient (pulse s) | recipient <- recipients newModuleState]
      ConjunctionType -> case moduleState newModuleState of
        Conjunction (ConjunctionState _m) -> if all (== High) $ Map.elems _m
        then [Signal (address s) recipient Low | recipient <- recipients newModuleState] 
        else [Signal (address s) recipient High | recipient <- recipients newModuleState]
    m' = Map.insert (address s) newModuleState m

updateConjunctions :: Map.Map String Module -> [(String, Module)] ->Map.Map String Module
updateConjunctions m [] = m
updateConjunctions m ((name, c):rest) = updateConjunctions m' rest
  where
    m' = checkFor (recipients c) m
    checkFor [] m = m
    checkFor (r:rs) m = case Map.lookup r m of
      Just old@(Module ConjunctionType (Conjunction (ConjunctionState _m)) _) -> checkFor rs $ Map.insert r (Module ConjunctionType (Conjunction (ConjunctionState $ Map.insert name Low _m)) (recipients old)) m
      _ -> checkFor rs m

pushUntil :: Signal -> Map.Map String Module -> Int -> Int
pushUntil s m n
  | s `elem` signals = n
  | otherwise = pushUntil s newM (n+1)
  where
    buttonPress = Signal "button" "broadcaster" Low
    (newM, signals) = handleSignal m [] [buttonPress]

main :: IO ()
main = do
  input <- fmap lines getContents
  let modules = map parseModule input
  let moduleMap = Map.fromList $ map parseModule input
  let moduleMap' = updateConjunctions moduleMap modules

  -- this solution is input specific

  let nvf = pushUntil (Signal "vf" "jz" High) moduleMap' 1
  print nvf

  let nrn = pushUntil (Signal "rn" "jz" High) moduleMap' 1
  print nrn

  let ndh = pushUntil (Signal "dh" "jz" High) moduleMap' 1
  print ndh

  let nmk = pushUntil (Signal "mk" "jz" High) moduleMap' 1
  print nmk

  print $ lcm nvf $ lcm nrn $ lcm ndh nmk

