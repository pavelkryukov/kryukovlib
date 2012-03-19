{-
 - algorithms/slae.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.SLAE
    (SLAESolver)
where

import KryukovLib.Types.Matrix (SLAE)
import KryukovLib.Types.Vector (Vector)

-- |SLAESolver is the function which get System and returns
-- its solution, i. e. Vector
type SLAESolver s t = SLAE s t -> Maybe (Vector s t)
