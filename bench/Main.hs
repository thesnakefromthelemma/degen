{-# LANGUAGE Haskell2010
    , AllowAmbiguousTypes
    , FlexibleContexts
    , KindSignatures
    , RankNTypes
    , ScopedTypeVariables
    , TypeApplications
    #-}

module Main
    ( main
    ) where


-- EXTERNAL IMPORTS --

import System.Environment
    ( getArgs )

import Data.Kind
    ( Type )

import Control.Monad

import qualified Data.Vector.Generic as VG
    ( Vector
    , fromList
    )

import qualified Data.Vector.Unboxed as VU
    ( Vector )

import System.Random.Stateful
    ( UniformRange
    , globalStdGen
    , uniformListRM
    )

import Text.Read
    ( readMaybe )


-- INTERNAL IMPORTS --

import HyperVector
    ( HyperVector
        ( HyperVector )
    )

import qualified Sid
    ( sinkUnsafe )

import qualified Archer
    ( sinkUnsafe )


-- GENERATE RANDOM VECTOR OF DOUBLES OF GIVEN DIMENSION AND SINK IT --

type SinkType = forall (v :: Type -> Type) a. (VG.Vector v a, VG.Vector v (Int, a), VG.Vector v Int, Ord a) =>
    HyperVector v a -> HyperVector v Int

bench :: forall (v :: Type -> Type) a. (VG.Vector v a, VG.Vector v (Int, a), VG.Vector v Int, Show (v Int), Ord a, Num a, UniformRange a) =>
    SinkType -> Int -> IO ()
bench = \f -> \n ->
    print . f @v @a . HyperVector n . VG.fromList =<< uniformListRM (2 ^ n) (0, 1) globalStdGen

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dim, alg, itr] -> case (readMaybe @Int dim, readMaybe @Int itr) of
            (Just n, Just i)  -> case alg of
                    "Sid"    -> replicateM i (bench @VU.Vector @Double Sid.sinkUnsafe n) >> pure ()
                    "Archer" -> replicateM i (bench @VU.Vector @Double Archer.sinkUnsafe n) >> pure ()
                    _        -> putStrLn "Second command line didn't match."
            _                 -> putStrLn "First and/or third command line argument couldn't be parsed into an Int."
        _               -> putStrLn "Incorrect number of command line arguments."
