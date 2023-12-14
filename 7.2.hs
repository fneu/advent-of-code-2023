import Data.List

data Type = HighCard | OnePair | TwoPairs | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Eq, Ord, Show) -- Ord: ordering based on the order of the constructors

data CardValue = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Queen | King | Ace
  deriving (Eq, Ord, Show) -- Ord: ordering based on the order of the constructors

data Hand = Hand { handType :: Type, handValue :: [CardValue], bid :: Int}
  deriving (Eq, Show)

instance Ord Hand where
  compare c1 c2 = compare (handType c1, handValue c1) (handType c2, handValue c2)

parseValue :: Char -> CardValue
parseValue c = case c of
  'J' -> Joker
  '2' -> Two
  '3' -> Three
  '4' -> Four
  '5' -> Five
  '6' -> Six
  '7' -> Seven
  '8' -> Eight
  '9' -> Nine
  'T' -> Ten
  'Q' -> Queen
  'K' -> King
  'A' -> Ace

toType :: [CardValue] -> Type
toType values =
  case sort $ map length . group . sort $ filter (/= Joker) values of
    [5] -> FiveOfAKind
    [1, 4] -> FourOfAKind
    [2, 3] -> FullHouse
    [1, 1, 3] -> ThreeOfAKind
    [1, 2, 2] -> TwoPairs
    [1, 1, 1, 2] -> OnePair
    [1, 1, 1, 1, 1] -> HighCard
    -- one joker, 4 cards to match
    [4] -> FiveOfAKind
    [1, 3] -> FourOfAKind
    [2, 2] -> FullHouse
    [1, 1, 2] -> ThreeOfAKind
    [1, 1, 1, 1] -> OnePair
    -- 2 jokers, 3 cards to match
    [3] -> FiveOfAKind
    [1, 2] -> FourOfAKind
    [1, 1, 1] -> ThreeOfAKind
    -- 3 jokers
    [2] -> FiveOfAKind
    [1, 1] -> FourOfAKind
    -- 4 and 5
    _ -> FiveOfAKind

parseHand :: String -> Hand
parseHand line =
  let cardValues = map parseValue $ head $ words line in
    Hand {
      handType = toType cardValues,
      handValue = cardValues,
      bid = read $ last $ words line
    }

main :: IO ()
main = do
  input <- fmap lines getContents
  let hands = sort $ map parseHand input
  print $ sum $ zipWith (\ hand factor -> bid hand * factor) hands [1..]
