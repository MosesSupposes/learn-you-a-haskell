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

-- Here's how we can model the recursive List type
data List a
  = Empty
  | Cons (List a) deriving (Show, Read, Eq, Ord)

data Tree a
  = EmptyTree
  | Node a (Tree a) (Tree a)
  deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right) | x == a = Node x left right
                                 | x < a  = Node a (treeInsert x left) right
                                 | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x tree = case tree of
  EmptyTree -> False
  (Node a left right) | x == a -> True
                      | x < a  -> treeElem x left
                      | x > a  -> treeElem x right

-- We can use a fold to insert items into our tree.
numsTree :: Tree Int
numsTree = let nums = [8, 6, 4, 1, 7, 3, 5] in foldr treeInsert EmptyTree nums

------------------------------------------------------------------------------
-- Typeclasses 102

{-| This is how you define the Eq typeclass
    class Eq a where  
      (==) :: a -> a -> Bool  
      (/=) :: a -> a -> Bool  
      x == y = not (x /= y)  
      x /= y = not (x == y)   
-}

data TrafficLight = Red | Yellow | Green

-- Let's manually derive the Eq and Show typeclasses for our new type:

instance Eq TrafficLight where
  Red    == Red    = True
  Green  == Green  = True
  Yellow == Yellow = True
  _      == _      = False

instance Show TrafficLight where
  show Red    = "Red light"
  show Yellow = "Yellow light"
  show Green  = "Green light"

{-| You can also make typeclasses that are subclasses of other typeclasses. 
    The class declaration for Num is a bit long, but here's the first part:

    class (Eq a) => Num a where  
      ...    
-}

{-| Most of the times, class constraints in class declarations are used 
    for making a typeclass a subclass of another typeclass and class 
    constraints in instance declarations are used to express requirements 
    about the contents of some type. For instance, here we required the 
    contents of the Maybe to also be part of the Eq typeclass:
  
    instance (Eq m) => Eq (Maybe m) where  
      Just x == Just y = x == y  
      Nothing == Nothing = True  
      _ == _ = False  
-}

{-| Here's how to Functor typeclass is implemented:

    class Functor f where  
      fmap :: (a -> b) -> f a -> f b  

    Here are some examples of instances derived from the Functor type

    instance Functor [] where  
      fmap = map  

    instance Functor Maybe where  
      fmap f (Just x) = Just (f x)  
      fmap f Nothing = Nothing
-}

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x leftsub rightsub) =
    Node (f x) (fmap f leftsub) (fmap f rightsub)



