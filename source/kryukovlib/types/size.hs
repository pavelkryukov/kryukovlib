{-
 - types/table.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Types.Size
    (Zero(..), Succ(..), One, Two, Three, Four, Five, Six, Seven, Eight)
where

-- Type pseudonumbers for dimensions of Vectos and Matrices
data Zero   = Zero
data Succ a = Succ a
type One    = Succ Zero
type Two    = Succ One
type Three  = Succ Two
type Four   = Succ Three
type Five   = Succ Four
type Six    = Succ Five
type Seven  = Succ Six
type Eight  = Succ Seven