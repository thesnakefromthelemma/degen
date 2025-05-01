{-# LANGUAGE Haskell2010
    , ScopedTypeVariables
    #-}

module Misc 
    ( minOn
    ) where


-- MISC UTILITY (WHY ISNT THIS IN base?) --

minOn :: forall a b. Ord b => (a -> b) -> a -> a -> a
minOn = \f -> \a0 -> \a1 -> case compare (f a0) (f a1) of
    GT -> a1
    _  -> a0
