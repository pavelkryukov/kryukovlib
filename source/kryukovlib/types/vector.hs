{-
 - types/vector.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
module KryukovLib.Types.Vector
    (Vector(..), mapV, mapVV, basis, definedBasis, vtl)
where

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Semigroup
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Size

data Vector s t = Vector [t] deriving (Eq, Read)

-- Printer
instance (Show t) => Show (Vector s t) where
    show (Vector v) = concat [show l ++ "\n" | l <- v]

-- Mapping over Vector
mapV :: (t -> b) -> Vector s t -> [b]
mapV f = map f . vtl

mapVV :: (t -> b) -> Vector s t -> Vector s b
mapVV f = Vector . map f . vtl

-- Canonical basis in n-dimensional space (columns of identity matrix)
basis :: (LAO (Vector s t), Semigroup t) => [Vector s t]
basis = definedBasis $ mapVV (<+> iden) zero

-- Canonical basis created from specified vector.
-- Every basis vector is canonical basis vector
-- multiplied on corresponding element of specified vector.
definedBasis :: (Semigroup t) => Vector s t -> [Vector s t]
definedBasis (Vector v) =
    let
        n = length v - 1
    in
    map
        (\j -> Vector $ 
                ((replicate j zero) ++
                  [v !! j] ++
                  (replicate (n - j) zero)))
        [0..n]

-- Vector to list convertor
vtl :: Vector s t -> [t]
vtl (Vector v) = v

-- Scalar multiplication
instance (Semigroup t) 
        => CrossMult (Vector s t) (Vector s t) t where
    (\*\) (Vector a) (Vector b) = laosum $ zipWith (<*>) a b

-- Multiplication to number
instance (Semigroup t) =>
        CrossMult t (Vector s t) (Vector s t) where
    a \*\ v = mapVV (a <*>) v

instance (Semigroup t) =>
        CrossMult (Vector s t) t (Vector s t) where
    v \*\ a = mapVV (<*> a) v

instance (LAO t) => LAO (Vector Zero t) where
    norm1 _ = 0
    norm2 _ = 0
    euclid _ = 0
    zero = Vector []
    (<+>) _ _ = Vector []
    (<->) _ _ = Vector []

instance forall t s.
        (LAO (Vector s t), LAO t) => LAO (Vector (Succ s) t) where
    zero = Vector $ zero : vtl (zero::Vector s t)
    norm1 = maximum . (mapV norm1)
    norm2 =  sum . (mapV norm2)
    euclid = sum . mapV euclid
    (Vector a) <+> (Vector b) = Vector $ zipWith (<+>) a b
    (Vector a) <-> (Vector b) = Vector $ zipWith (<->) a b