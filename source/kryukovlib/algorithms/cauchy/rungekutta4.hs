{-
 - algorithms/cauchy/rungekutta4.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE ScopedTypeVariables #-}
module KryukovLib.Algorithms.Cauchy.RungeKutta4
    (rungekutta4)
where

import Prelude hiding (Num(..))

import KryukovLib.Generic.ListFunctions (diffgrid, midgrid, superzip)

import KryukovLib.Classes.LAO
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Table (zipTable)

import KryukovLib.Algorithms.Cauchy

-- |Common fourth-order Runge–Kutta method for solving Cauchy's problem
rungekutta4 :: forall t u.
    (NumberMult t u) => CauchySolver t u
rungekutta4 func nodes base = zipTable nodes values
    where
        half   = map ((0.5::t) \*\)
        sixth  = map ((1/6::t) \*\)

        diffnodes  = diffgrid nodes
        shiftnodes = tail nodes
        midnodes   = midgrid nodes

        vk1 = values + (half k1)
        vk2 = values + (half k2)
        vk3 = values + k3

        k1   = zipWith (\*\) diffnodes (zipWith func nodes   values)
        k2   = zipWith (\*\) diffnodes (zipWith func midnodes   vk1)
        k3   = zipWith (\*\) diffnodes (zipWith func midnodes   vk2)
        k4   = zipWith (\*\) diffnodes (zipWith func shiftnodes vk3)

        ka   = sixth $ superzip (+) [k1, k2, k2, k3, k3, k4]

        values = base : (values + ka)