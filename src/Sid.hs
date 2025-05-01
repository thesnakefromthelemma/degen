{-# LANGUAGE Haskell2010
    , FlexibleContexts
    , KindSignatures
    , MultiParamTypeClasses
    , ScopedTypeVariables
    #-}

module Sid
    ( sinkUnsafe
    , sinkSafe
    , sinkMaybe
    ) where


-- EXTERNAL IMPORTS --

import GHC.Stack
    ( HasCallStack )

import Data.Kind
    ( Type )

import Data.Function
    ( fix )

import Data.Bits
    ( complementBit )

import qualified Data.Vector.Generic as VG
    ( Vector
    , generate
    , length
    , unsafeIndex
    )


-- INTERNAL IMPORTS --

import Misc
    ( minOn )

import HyperVector
    ( HyperVector
        ( HyperVector )
    , isHyperVector
    )


-- SID'S ALGORITHM --

{-# INLINE gradient #-}
gradient :: forall (v :: Type -> Type) a. (VG.Vector v a, VG.Vector v Int, Ord a) =>
    HyperVector v a -> HyperVector v Int
gradient = \(HyperVector n va) ->
    HyperVector n $ VG.generate (2 ^ n) (\i -> foldl' (minOn $ \i' -> VG.unsafeIndex va i') i $ fmap (complementBit i) [0..n-1])

{-# INLINE sinkUnsafe #-}
sinkUnsafe :: forall (v :: Type -> Type) a. (VG.Vector v a, VG.Vector v Int, Ord a) =>
    HyperVector v a -> HyperVector v Int
sinkUnsafe = \ha@(HyperVector n _) ->
    let HyperVector _ va' = gradient ha
    in  HyperVector n $ VG.generate (VG.length va') ( fix $ \r -> \i ->
            let i' = VG.unsafeIndex va' i
            in  case i == i' of
                    True  -> i
                    False -> r i'
          )


-- SMOOTHER API --

{-# INLINE sinkSafe #-}
sinkSafe :: forall (v :: Type -> Type) a. (HasCallStack, VG.Vector v a, VG.Vector v Int, Ord a) =>
    HyperVector v a -> HyperVector v Int
sinkSafe = \ha -> case isHyperVector ha of
    True  -> sinkUnsafe ha
    False -> error "Invalid argument passed to sinkSafe"

{-# INLINE sinkMaybe #-}
sinkMaybe :: forall (v :: Type -> Type) a. (VG.Vector v a, VG.Vector v Int, Ord a) =>
    HyperVector v a -> Maybe (HyperVector v Int)
sinkMaybe = \ha -> case isHyperVector ha of
    True  -> Just $ sinkUnsafe ha
    False -> Nothing
