{-
 - classes/crossmult.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, MonoLocalBinds #-}
module KryukovLib.Classes.CrossMult
    (CrossMult(..), NumberMult)
where

import Prelude hiding (Num(..), Semigroup)

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Number
import KryukovLib.Classes.Semigroup

-- |Cross Multiplication class
-- Allows multiplication of one type to another to get the third one
-- E. g. mulitplication Matrix to Vector and return Vector
class CrossMult a b c where
    -- Cross Multiplication operator
    (\*\) :: a -> b -> c

instance (Semigroup a, Number a) => CrossMult a a a where
    (\*\) = (*)
    
class (Number t, LAO f, CrossMult t f f) => NumberMult t f
instance (Number t, LAO f, CrossMult t f f) => NumberMult t f
