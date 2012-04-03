{-
 - algorithms/cauchy/euler.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.Cauchy.Euler
    (euler)
where

import KryukovLib.Generic.ListFunctions (diffgrid)

import KryukovLib.Classes.LAO
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Table (zipTable)

import KryukovLib.Algorithms.Cauchy

-- |Simpliest Euler method of solving Cauchy's problem
euler :: (CrossMult t u u, LAO u, Num t) => CauchySolver t u
euler func nodes base = zipTable nodes values
    where
        funcvals  = zipWith func nodes values
        diffnodes = diffgrid nodes
        deltas    = zipWith (\*\) diffnodes funcvals
        values    = base : (values <+> deltas)