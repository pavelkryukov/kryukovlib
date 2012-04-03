{-
 - generic/peano.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Generic.Peano
    (Succ(..), One(..), Peano, Two, Three, Four, Five, Six, Seven, Eight)
where

import Prelude hiding (Num(..))

-- |Type Peano Numbers for dimensions of Vectos and Matrices
data One    = One
data Succ a = Succ a

class    Peano a
instance Peano One
instance Peano a => Peano (Succ a)

type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three
type Five  = Succ Four
type Six   = Succ Five
type Seven = Succ Six
type Eight = Succ Seven