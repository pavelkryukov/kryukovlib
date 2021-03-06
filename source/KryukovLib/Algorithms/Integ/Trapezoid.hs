{-
 - algorithms/integ/trapezoid.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE ScopedTypeVariables, MonoLocalBinds #-}
module KryukovLib.Algorithms.Integ.Trapezoid
    (trapezoid)
where

import Prelude hiding (Num(..))

import KryukovLib.Generic.ListFunctions (diffgrid)

import KryukovLib.Classes.LAO
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Table (unTable)

import KryukovLib.Algorithms.Integ

-- |Trapezoid integral method
trapezoid :: forall t f.
    (NumberMult t f) => Integ t f
trapezoid f =
    let
        (nodes, values') = unTable f
        diffnodes = diffgrid nodes
        values  = zipWith (+) (tail values') values'
    in
        (0.5::t) \*\ ((laosum::[f]->f) (zipWith (\*\) diffnodes values))