{-# LANGUAGE Haskell2010
    , FlexibleContexts
    , KindSignatures
    , MultiParamTypeClasses
    , ScopedTypeVariables
    #-}

module Archer
    ( sinkSafe
    , sinkUnsafe
    , sinkMaybe
    ) where


-- EXTERNAL IMPORTS --

import GHC.Stack
    ( HasCallStack )

import Data.Kind
    ( Type )

import Control.Monad
    ( forM_ )

import Control.Monad.ST
    ( runST )

import Data.Bits
    ( complementBit )

import qualified Data.Vector.Generic as VG
    ( Vector
    , length
    , unsafeIndex
    , map
    , indexed
    , freeze
    , thaw
    )

import qualified Data.Vector.Generic.Mutable as VGM
    ( unsafeNew
    , unsafeWrite
    , unsafeRead
    )

import qualified Data.Vector.Algorithms.Merge as Merge
    ( sortBy )


-- INTERNAL IMPORTS --

import Misc
    ( minOn )

import HyperVector
    ( HyperVector
        ( HyperVector )
    , isHyperVector
    )


-- ARCHER'S ALGORITHM --

{-# INLINE invertUnsafe #-}
invertUnsafe :: forall (v :: Type -> Type). VG.Vector v Int =>
    v Int -> v Int
invertUnsafe = \vn -> runST $ do
    let len = VG.length vn
    vn' <- VGM.unsafeNew len
    forM_ [0..len-1] $ \i ->
        VGM.unsafeWrite vn' (VG.unsafeIndex vn i) i
    VG.freeze vn'

{-# INLINE topoUnsafe #-}
topoUnsafe :: forall (v :: Type -> Type) a. (VG.Vector v a, VG.Vector v (Int, a), VG.Vector v Int, Ord a) =>
    v a -> (v Int, v Int)
topoUnsafe = \va ->
    let vn = VG.map fst $ runST $ do
            vt <- VG.thaw $ VG.indexed va
            Merge.sortBy (\t0 -> \t1 -> compare (snd t0) (snd t1)) vt
            VG.freeze vt
    in  (vn, invertUnsafe vn)

{-# INLINE sinkUnsafe #-}
sinkUnsafe :: forall (v :: Type -> Type) a. (VG.Vector v a, VG.Vector v (Int, a), VG.Vector v Int, Ord a) =>
    HyperVector v a -> HyperVector v Int
sinkUnsafe = \(HyperVector n va) -> HyperVector n $ runST $ do
    let len = VG.length va
        (vn0, vn1) = topoUnsafe va -- (RANK -> INDEX, INDEX -> RANK)
    vn <- VGM.unsafeNew len
    forM_ [0..len-1] $ \j ->
        let i = VG.unsafeIndex vn0 j
            i' = foldl' (minOn $ \i'' -> VG.unsafeIndex vn1 i'') i $ fmap (complementBit i) [0..n-1]
        in  case i == i' of
                True  -> VGM.unsafeWrite vn i i
                False -> VGM.unsafeWrite vn i =<< VGM.unsafeRead vn i'
    VG.freeze vn


-- SMOOTHER API --

{-# INLINE sinkSafe #-}  
sinkSafe :: forall (v :: Type -> Type) a. (HasCallStack, VG.Vector v a, VG.Vector v (Int, a), VG.Vector v Int, Ord a) =>
    HyperVector v a -> HyperVector v Int
sinkSafe = \ha -> case isHyperVector ha of
    True  -> sinkUnsafe ha
    False -> error "Invalid argument passed to sinkSafe"
    
{-# INLINE sinkMaybe #-}
sinkMaybe :: forall (v :: Type -> Type) a. (VG.Vector v a, VG.Vector v (Int, a), VG.Vector v Int, Ord a) =>
    HyperVector v a -> Maybe (HyperVector v Int)
sinkMaybe = \ha -> case isHyperVector ha of
    True  -> Just $ sinkUnsafe ha
    False -> Nothing
