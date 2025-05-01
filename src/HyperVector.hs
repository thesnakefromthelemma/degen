{-# OPTIONS_GHC -funbox-strict-fields #-}

{-# LANGUAGE Haskell2010
    , BangPatterns
    , GADTs
    , KindSignatures
    , ScopedTypeVariables
    , StandaloneDeriving
    #-}

module HyperVector
    ( HyperVector
        ( HyperVector )
    , isHyperVector
    , hyperVector
    , hyperVectorMaybe
    ) where


-- EXTERNAL IMPORTS --

import GHC.Stack
    ( HasCallStack )

import Data.Kind
    ( Type )

import qualified Data.Vector.Generic as VG
    ( Vector
    , length
    )


-- DATA HyperVector --

data HyperVector :: (Type -> Type) -> Type -> Type where
    HyperVector :: forall (v :: Type -> Type) a. VG.Vector v a =>
        !Int -> !(v a) -> HyperVector v a

deriving instance forall (v :: Type -> Type) a. (VG.Vector v a, Eq (v a)) =>
    Eq (HyperVector v a)

deriving instance forall (v :: Type -> Type) a. (VG.Vector v a, Show (v a)) =>
    Show (HyperVector v a)

isHyperVector :: forall (v :: Type -> Type) a. VG.Vector v a =>
    HyperVector v a -> Bool
isHyperVector = \(HyperVector n va) ->
    (2 ^ n) == VG.length va

hyperVector :: forall (v :: Type -> Type) a. (HasCallStack, VG.Vector v a) =>
    Int -> (v a) -> HyperVector v a
hyperVector = \n -> \va ->
    let ha :: HyperVector v a
        ha = HyperVector n va
    in  case isHyperVector ha of
            True  -> ha
            False -> error $ "Dimensionality " ++ show n ++ " does not match length " ++ show (VG.length va)

hyperVectorMaybe :: forall (v :: Type -> Type) a. VG.Vector v a =>
    Int -> (v a) -> Maybe (HyperVector v a)
hyperVectorMaybe = \n -> \va ->
    let ha :: HyperVector v a
        ha = HyperVector n va
    in  case isHyperVector ha of
            True  -> Just ha                
            False -> Nothing
