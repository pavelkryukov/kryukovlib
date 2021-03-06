{-
 - generic/listfunctions.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE MonoLocalBinds #-}
module KryukovLib.Generic.ListFunctions
    (merge, hgrid, chebPolyZeroes, tail2,
     evenl, oddl, diffgrid, shift, midgrid, superzip)
where

import Prelude hiding (Num(..))
import qualified Prelude as P

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Number
import KryukovLib.Classes.Semigroup

-- |Merge of two lists-- [a,a,a,a,a] [b,b,b,b,b] -> [a,b,a,b,a,b,a,b]
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- |Even elements of list
evenl :: [t] -> [t]
evenl (x:xs) = x : (oddl xs)
evenl [] = []

-- |Odd elements of list
oddl :: [t] -> [t]
oddl (_:xs) = evenl xs
oddl [] = []

-- |List without 2 first elements
tail2 :: [t] -> [t]
tail2 = tail . tail

-- |Take differences between list elements
diffgrid :: (LAO t) => [t] -> [t]
diffgrid a = zipWith (-) (tail a) a

-- |Even grid from a to b
hgrid :: (Number t) => (t, t) -> Int -> [t]
hgrid (a,b) n =
    let
        h = ((b - a)/(P.fromIntegral (n P.- 1)))
    in
        take n (iterate (+ h) a)

-- |Shift of list with copying of first element
shift :: [t] -> [t]
shift a = (head a) : a

-- |Returns lists of pair mediums of list elements
midgrid :: (Number t) => [t] -> [t]
midgrid a = map (/2) (zipWith (+) (tail a) a)

-- |Zips list of lists into list with selected function
superzip :: (t -> t -> t) -> [[t]] -> [t]
superzip _ []     = undefined
superzip _ [a]    = a
superzip f (a:bs) = zipWith f a (superzip f bs)

-- |Zeroes of n-th Chebyshev polynom
chebPolyZeroes :: (Number t) => Integer -> [t]
chebPolyZeroes n =
    map
        (\i -> cos(((2*(P.fromInteger i) + 1) / (2 * (P.fromInteger n) + 1)) * pi))
        [0..n]