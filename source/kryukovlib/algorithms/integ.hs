{-
 - algorithms/integ.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.Integ
    (Integ)
where

import KryukovLib.Types.Table (Table)

type Integ t f = (Table t f) -> f

