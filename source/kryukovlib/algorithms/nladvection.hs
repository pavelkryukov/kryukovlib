{-
 - algorithms/nladvection.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.NLAdvection
    (NLAdvection)
where

import KryukovLib.Types.Table (Table)

-- |NLAdvection solves equation du/dt + d(u^2/2)/dx = 0
type NLAdvection x t f = Table x f -> Table t (Table x f)
