{-
 - types/matrix.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE ScopedTypeVariables, UndecidableInstances, FlexibleInstances #-}
module KryukovLib.Types.Matrix
    (Matrix(..), SqrMatrix, SLAE(..),
    takeDiag, diag, trans)
where

import Data.List (transpose)

import KryukovLib.Generic.Debug (notImpl)

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Semigroup
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Size
import KryukovLib.Types.Vector

-- Matrix s1 x s2 type with LAO, Semigroup and CrossMult operations
data Matrix s1 s2 t = Matrix [Vector s2 t] deriving (Eq, Read)

-- Square matrix synonym
type SqrMatrix s t = Matrix s s t

-- SLAE datatype
data SLAE s t = SLAE (SqrMatrix s t) (Vector s t)

-- Internal functions
matrixToDoubleList :: (Matrix s1 s2 t) -> [[t]]
matrixToDoubleList (Matrix m1) = map vtl m1

mtl :: Matrix s1 s2 t -> [Vector s2 t]
mtl (Matrix m) = m

-- Matrix printer
instance (Show t) => Show (Matrix s1 s2 t) where
    show (Matrix m) =
        concat $
            map (\l -> concat (mapV (\i -> show i ++ " ") l) ++ "\n") m

-- Matrix transponer
trans :: (Matrix s1 s2 t) -> (Matrix s2 s1 t)
trans = Matrix . map (Vector) . transpose . matrixToDoubleList

-- Selects main diagonal in matrix
takeDiag :: SqrMatrix s t -> Vector s t
takeDiag (Matrix a) =
    Vector $ map (\i -> (vtl (a !! i)) !! i) [0..((length a) - 1)]

-- Creates diagonal matrix from vector
diag :: (Semigroup t) => Vector s t -> SqrMatrix s t
diag = Matrix . definedBasis

-- Multiplication of A to transposed A
--transMult :: (Semigroup t) => (Matrix s1 s2 t) -> (SqrMatrix s1 t)
--transMult (Matrix a) = Matrix (map (\a1 -> Vector (map (\*\ a1) a)) a)

-- Linear operations on Matrix
instance (LAO t) => LAO (Matrix Zero s t) where
    zero = Matrix []
    norm1 _ = 0
    norm2 _ = 0
    euclid _ = 0
    (<+>) _ _ = zero
    (<->) _ _ = zero

instance forall t s1 s2.
        (LAO (Matrix s1 s2 t),
         LAO (Vector s2 t),
         LAO (Matrix s2 (Succ s1) t)) =>
         LAO (Matrix (Succ s1) s2 t) where
    zero = Matrix $ zero : mtl (zero::(Matrix s1 s2 t))
    norm1 = norm2 . trans
    norm2 (Matrix m) = maximum $ map (norm2) m
--  euclid m = maximum $ map norm3 (eigenvalues (transMult m))
    euclid = notImpl
    (Matrix a) <+> (Matrix b) = Matrix $ zipWith (<+>) a b
    (Matrix a) <-> (Matrix b) = Matrix $ zipWith (<->) a b

-- Multiplication Matrix on Number
instance (Semigroup t) => CrossMult (Matrix s1 s2 t) t (Matrix s1 s2 t) where
    (Matrix a) \*\ n = Matrix $ map (n \*\) a

instance (Semigroup t) => CrossMult t (Matrix s1 s2 t) (Matrix s1 s2 t) where
    n \*\ a = a \*\ n

-- Multiplication Matrix on Vector
instance (Semigroup t) => 
        CrossMult (Matrix s1 s2 t) (Vector s2 t) (Vector s1 t) where
    (Matrix a) \*\ b = Vector $ map (\*\ b) a

-- Multiplication of Matrix on Matrix
instance (Semigroup t) =>
    CrossMult (Matrix s1 s2 t) (Matrix s2 s3 t) (Matrix s1 s3 t) where
    (Matrix a) \*\ b =
        Matrix $ map (\a1 -> Vector (map (\*\ a1) ((mtl . trans) b))) a

-- Identity matrix and multiplication of square matrices
instance (Semigroup t, LAO (SqrMatrix s t), LAO (Vector s t)) =>
        Semigroup (SqrMatrix s t) where
    iden = Matrix basis
    (Matrix a) <*> b =
        Matrix (map (\a1 -> Vector (map (\*\ a1) ((mtl . trans) b))) a)