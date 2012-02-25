{-
 - algorithms/cauchy.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.Cauchy
    (CauchySolver)
where

import KryukovLib.Types.Table (Table)

-- CauchySolver solves Cauchy's problem dU/dt = F(U,t)
-- First argument is F function, second is grid
-- and the third one is value of U in first point of grid
type CauchySolver t u = (t -> u -> u) -> [t] -> u -> (Table t u)