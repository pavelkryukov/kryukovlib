{-
 - algorithms/cauchy/heun.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.Cauchy.Heun
    (heun)
where

import KryukovLib.Generic.ListFunctions (diffgrid)

import KryukovLib.Classes.LAO
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Table (zipTable)

import KryukovLib.Algorithms.Cauchy

-- Heun method of solving Cauchy's problem (with correction)
heun :: (CrossMult t u u, LAO u, Fractional t) => CauchySolver t u
heun func nodes base = zipTable nodes values'
    where
        funcvals   = zipWith func nodes values
        funcvals2  = zipWith func nodes values'
        funcvals'  = zipWith (<+>) funcvals2 (tail funcvals)
        diffnodes  = diffgrid nodes
        diffnodes' = map (/2) diffnodes
        deltas     = zipWith (\*\) diffnodes  funcvals
        deltas'    = zipWith (\*\) diffnodes' funcvals'
        values     = base : zipWith (<+>) values deltas
        values'    = base : zipWith (<+>) values deltas'