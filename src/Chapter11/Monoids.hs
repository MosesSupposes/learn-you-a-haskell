module Chapter11.Monoids where

-- This is how the Monoid typeclass is defined:
-- * class Monoid m where  
-- *    mempty :: m  
-- *    mappend :: m -> m -> m  
-- *    mconcat :: [m] -> m  
-- *    mconcat = foldr mappend mempty

-- Because mconcat has a default implementation, we get it for free when we 
-- make something an instance of Monoid.

-- Lists are Monoids. Take a look: 
-- * instance Monoid [a] where  
-- *    mempty = []  
-- *    mappend = (++)  

