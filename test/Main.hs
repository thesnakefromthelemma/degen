{-# LANGUAGE Haskell2010
    , AllowAmbiguousTypes
    , FlexibleContexts
    , KindSignatures
    , ScopedTypeVariables
    , TypeApplications
    #-}

module Main
    ( main
    ) where


-- EXTERNAL IMPORTS --

import System.Environment
    ( getArgs )

import System.Exit
    ( exitSuccess
    , exitFailure
    )

import Data.Kind
    ( Type )

import Control.Monad
   ( replicateM )

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


-- SUPERFICIAL IMPORTS --

import HyperVector
    ( HyperVector
        ( HyperVector )
    )

import qualified Sid
    ( sinkUnsafe )

import qualified Archer
    ( sinkUnsafe )


-- GENERATE RANDOM VECTOR OF DOUBLES OF GIVEN DIMENSION AND SINK IT --

test :: forall (v :: Type -> Type) a. (VG.Vector v a, VG.Vector v (Int, a), VG.Vector v Int, Eq (v Int), Ord a, Num a, UniformRange a) =>
    Int -> IO Bool
test = \n -> do
    ha <- HyperVector @v @a n . VG.fromList <$> uniformListRM (2 ^ n) (0, 1) globalStdGen
    pure $ Sid.sinkUnsafe ha == Archer.sinkUnsafe ha

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dim, itr] -> case (readMaybe @Int dim, readMaybe @Int itr) of
            (Just n, Just i)  -> do
                st <- replicateM i (test @VU.Vector @Double n)
                case and st of
                    True  -> putStrLn "Tests passed" >> exitSuccess
                    False -> putStrLn "Tests failed" >> exitFailure
            _                 -> putStrLn "First and/or second command line argument couldn't be parsed into an Int." >> exitSuccess
        _               -> putStrLn "Incorrect number of command line arguments." >> exitSuccess

