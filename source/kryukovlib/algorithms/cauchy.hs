{-
 - algorithms/cauchy.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.Cauchy
    (CauchySolver)
where

import KryukovLib.Types.Table

type CauchySolver t u = 
    (t -> u -> u) -> [t] -> u -> (Table t u)