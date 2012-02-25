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

-- Integ operator counts integral over table function
-- Table function should be sorted
type Integ t f = (Table t f) -> f

