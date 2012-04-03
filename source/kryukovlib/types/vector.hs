{-
 - types/vector.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE
    ScopedTypeVariables, 
    FlexibleInstances, 
    ExistentialQuantification,
    MultiParamTypeClasses,
    FlexibleContexts,
    IncoherentInstances #-}
module KryukovLib.Types.Vector
    (Vector(..), mapV, mapVV, basis, definedBasis, vtl)
where

import Prelude hiding (Num(..))
import qualified Prelude as P

import KryukovLib.Generic.Peano

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Semigroup
import KryukovLib.Classes.CrossMult

-- |s-dimension Vector type with support of LAO and CrossMult operations
data Vector s t = (Peano s) => Vector [t]

instance (Eq t) => Eq (Vector s t) where
    (==) (Vector v1) (Vector v2) = and (zipWith (==) v1 v2)
    (/=) (Vector v1) (Vector v2) = or (zipWith (/=) v1 v2)

-- |Printer
instance (Show t) => Show (Vector s t) where
    show (Vector v) = concat [show l ++ "\n" | l <- v]

-- |Map over Vector with return of list
mapV :: (t -> b) -> Vector s t -> [b]
mapV f = map f . vtl

-- |Map over Vector with return of Vector
mapVV :: (Peano s) => (t -> b) -> Vector s t -> Vector s b
mapVV f = Vector . map f . vtl

-- |Canonical basis in n-dimensional space (columns of identity matrix)
basis :: (Peano s, LAO (Vector s t), Semigroup t) => [Vector s t]
basis = definedBasis $ mapVV (+ iden) zero

-- |Canonical basis created from specified vector.
-- Every basis vector is canonical basis vector
-- multiplied on corresponding element of specified vector.
definedBasis :: (Semigroup t) => Vector s t -> [Vector s t]
definedBasis (Vector v) =
    let
        n = length v P.- 1
    in
    map
        (\j -> Vector $ 
                ((replicate j zero) ++
                  [v !! j] ++
                  (replicate (n P.- j) zero)))
        [0..n]

-- |Vector to list convertor
vtl :: Vector s t -> [t]
vtl (Vector v) = v

-- |Scalar multiplication
instance (Semigroup t) 
        => CrossMult (Vector s t) (Vector s t) t where
    (\*\) (Vector a) (Vector b) = laosum $ zipWith (*) a b

-- |Multiplication to number
instance (Peano s, Semigroup t) =>
        CrossMult t (Vector s t) (Vector s t) where
    a \*\ v = mapVV (a *) v

instance (Peano s, Semigroup t) =>
        CrossMult (Vector s t) t (Vector s t) where
    v \*\ a = mapVV (* a) v

instance (LAO t) => LAO (Vector One t) where
    zero = Vector $ [zero]
    norm1 = maximum . (mapV norm1)
    norm2 =  sum . (mapV norm2)
    euclid = sum . mapV euclid
    (Vector a) + (Vector b) = Vector $ a + b
    (Vector a) - (Vector b) = Vector $ a - b

instance forall t s.
        (Peano s, LAO (Vector s t), LAO t) => LAO (Vector (Succ s) t) where
    zero = Vector $ zero : vtl (zero::Vector s t)
    norm1 = maximum . (mapV norm1)
    norm2 =  sum . (mapV norm2)
    euclid = sum . mapV euclid
    (Vector a) + (Vector b) = Vector $ a + b
    (Vector a) - (Vector b) = Vector $ a - b