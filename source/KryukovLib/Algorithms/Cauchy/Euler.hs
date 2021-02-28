{-
 - algorithms/cauchy/euler.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE MonoLocalBinds #-}
module KryukovLib.Algorithms.Cauchy.Euler
    (euler)
where

import Prelude hiding (Num(..))

import KryukovLib.Generic.ListFunctions (diffgrid)

import KryukovLib.Classes.LAO
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Table (zipTable)

import KryukovLib.Algorithms.Cauchy

-- |Simpliest Euler method of solving Cauchy's problem
euler :: (NumberMult t u) => CauchySolver t u
euler func nodes base = zipTable nodes values
    where
        funcvals  = zipWith func nodes values
        diffnodes = diffgrid nodes
        deltas    = zipWith (\*\) diffnodes funcvals
        values    = base : zipWith (+) values deltas

