{-
 - algorithms/hopf.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.Hopf
    (Hopf)
where

import KryukovLib.Types.Table (Table)

-- |Solves equation du/dt + d(u^2/2)/dx = 0
type Hopf x t f = Table x f -> Table t (Table x f)
