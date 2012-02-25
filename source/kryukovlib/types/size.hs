{-
 - types/table.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Types.Size
    (Zero(..), Succ(..), One, Two, Three, Four)
where

data Zero = Zero
data Succ a = Succ a

type One   = Succ Zero
type Two   = Succ (Succ Zero)
type Three = Succ (Succ (Succ Zero))
type Four  = Succ (Succ (Succ (Succ Zero)))