{-
 - classes/semigroup.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
module KryukovLib.Classes.Semigroup
    (Semigroup(..))
where

import Prelude hiding (Num(..))
import qualified Prelude as P

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Number

-- |Class of types with multiplication and identity element
class (LAO a) => Semigroup a where
    -- Identity element
    iden :: a
    -- Multiplication
    (*) :: a -> a -> a
    -- Fibonacci range
    fibonacci :: [a]
    fibonacci = zero : iden : zipWith (+) fibonacci (tail fibonacci)
    -- Kroneker symbol
    kroneker :: (Eq t) => t -> t -> a
    kroneker m n
        | m == n    = iden
        | otherwise = zero
    -- Ackermann function
    ackermann :: (Eq a) => a -> a -> a
    ackermann m n = 
        case (m == zero, n == zero) of
            (True, _) -> n + iden
            (False, True) -> ackermann (m - iden) iden
            (False, False) -> ackermann (m - iden) (ackermann m (n - iden))

instance (Number a) => Semigroup a where
    iden = 1
    (*) = (P.*)