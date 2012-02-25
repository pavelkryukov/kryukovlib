{-
 - classes/semigroup.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module KryukovLib.Classes.Semigroup
    (Semigroup(..))
where

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Number

class (LAO a) => Semigroup a where
    iden :: a
    (<*>) :: a -> a -> a
    fibonacci :: [a]
    fibonacci = zero : iden : zipWith (<+>) fibonacci (tail fibonacci)
    kroneker :: (Eq t) => t -> t -> a
    kroneker m n
        | m == n    = iden
        | otherwise = zero
    ackermann :: (Eq a) => a -> a -> a
    ackermann m n = 
        case (m == zero, n == zero) of
            (True, _) -> n <+> iden
            (False, True) -> ackermann (m <-> iden) iden
            (False, False) -> ackermann (m <-> iden) (ackermann m (n <-> iden))

instance (Num a, Number a) => Semigroup a where
    iden = 1
    (<*>) = (*)