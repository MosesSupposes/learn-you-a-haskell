module Chapter13.MonadFunctions where

-- liftM is the same as fmap except that it operates on monads.
-- This is how liftM is implmented:

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= (\x -> return (f x))

-- Or alternatively, it can be defined with do notation, like so:

liftM' :: (Monad m) => (a -> b) -> m a -> m b
liftM' f m = do
    x <- m
    return (f x)

-- it turns out that just like fmap, <*> can also be implemented by using only what the Monad type class give us.
-- The ap function is basically <*>, only it has a Monad constraint instead of an Applicative one.
-- Here's its definition: 

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = m >>= (\x -> mf >>= (\f -> return (f x)))


-- Or alternatively, it can be defined with do notation, like so:

ap' :: (Monad m) => m (a -> b) -> m a -> m b
ap' mf m = do
    f <- mf
    x <- m
    return (f x)

-- The liftA2 function is a convenience function for applying a function between two applicative values.
-- It's defined simply like so:

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y

-- The liftM2 function does the same thing, only it has a Monad constraint. There also exist liftM3 and liftM4 and liftM5.

-- The join function.
-- If we have Just (Just 9), can we make that into Just 9?
-- It turns out that any nested monadic value can be flattened and that this is actually a property unique to monads.
-- For this, the join function exists. This is how it's defined

join :: (Monad m) => m (m a) -> m a
join mm = mm >>= (\m -> m)

-- Or alternatively with do syntax:

join' :: (Monad m) => m (m a) -> m a
join' mm = do
    m <- mm
    m

-- Here it is in action:
joinExample :: (Maybe Int, Maybe a, Maybe b)
joinExample =
    let m1 = join (Just (Just 9))
        m2 = join (Just Nothing)
        m3 = join Nothing
    in  (m1, m2, m3)
-- (Just 9, Nothing, Nothing)

joinExample2 :: [Int]
joinExample2 = join [[1, 2, 3], [4, 5, 6]]
-- [1,2,3,4,5,6]

--  Notice that `m >>= f` is always the same thing as `join (fmap f m)`

-- Take a look at the following function. It's the monad equivalent to `filter`.
-- * filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]


-- foldM
-- The monadic counterpart to foldl is foldM

-- Here's the type signature for foldl
-- * foldl :: (a -> b -> a) -> a -> [b] -> a  

-- And here's the signature for foldM
-- * foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a 

-- The value that the binary function returns is monadic and so the result of the whole fold is monadic as well. 
