module Chapter8.OurTypes
  ( Point(..)
  , Shape(..)
  , surface
  , nudge
  ) where

data Point = Point Float Float
  deriving Show

data Shape
  = Circle Point Float
  | Rectangle Point Point
  deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) =
  (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
  Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

data Person = Person
  { firstName   :: String
  , lastName    :: String
  , age         :: Int
  , height      :: Float
  , phoneNumber :: String
  , flavor      :: String
  }
  deriving Show

data Car = Car
  { company :: String
  , model   :: String
  , year    :: Int
  }
  deriving Show

c :: Car
c = Car { company = "Toyota", model = "Camry", year = 2016 }

-- * Note that each field from a record is made into a function. Observe:
toyota :: String
toyota = company c -- Prints "Toyota"

-- This is how you pattern match on a record
tellCar :: Car -> String
tellCar Car { company = c, model = m, year = y } =
  "This " ++ c ++ " " ++ m ++ " was made in" ++ show y

-- You can add a typeclass constraint to a type like so:
-- * data (Ord k) => Map k v = ...
-- However, it is a very strong convention to not ever do this.

-- The "Type" keyword is used to make type synonnyms. Observe:

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

phonebook :: PhoneBook
phonebook =
  [ ("betty"  , "555-2938")
  , ("bonnie" , "452-2928")
  , ("patsy"  , "493-2928")
  , ("lucille", "205-2928")
  , ("wendy"  , "939-8282")
  , ("penny"  , "853-2492")
  ]

-- Isn't this much more readable with type synonyms?
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

-- If we decided not to use type synonyms, our function would have 
-- a type of String -> String -> [(String,String)] -> Bool

-- Type synonyms can also be parameterized. Observe:
type AssocList k v = [(k, v)]

lookup' :: (Ord k) => k -> AssocList k v -> Maybe v
lookup' key assocList =
  let possibleMatch = safeHead . filter (\(k, _) -> k == key) $ assocList
  in  case possibleMatch of
        Just (_, foundValue) -> Just foundValue
        Nothing              -> Nothing
  where safeHead list = if length list > 0 then Just $ head list else Nothing

data LockerState
  = Taken
  | Free
  deriving (Show, Eq)

type Code = String

type LockerMap = AssocList Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case lookup' lockerNumber map of
  Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist."
  Just (state, code) -> if state /= Taken
    then Right code
    else Left ("Locker " ++ show lockerNumber ++ " is already taken!")

-- Here's our Locker suite of types in action:
lockers :: LockerMap
lockers =
  [ (100, (Taken, "ZD39I"))
  , (101, (Free, "JAH3I"))
  , (103, (Free, "IQSA9"))
  , (105, (Free, "QOTSA"))
  , (109, (Taken, "893JJ"))
  , (110, (Taken, "99292"))
  ]

-- lockerLookup 101 lockers 
-- ? Right "JAH3I" 

-- lockerLookup 100
-- ! Left "Locker 100 is already taken!"

-- lockerLookup 102
-- ! Left "Locker number 102 doesn't exist!"
