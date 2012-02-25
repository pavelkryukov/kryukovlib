{-
 - algorithms/integ/trapecies.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE ScopedTypeVariables #-}
module KryukovLib.Algorithms.Integ.Trapecies
    (trapInteg)
where

import KryukovLib.Generic.ListFunctions (diffgrid)

import KryukovLib.Classes.LAO
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Table (unTable)

import KryukovLib.Algorithms.Integ

-- Intergal of table function
trapInteg :: forall t f.
    (CrossMult t f f, Fractional t, LAO f) => Integ t f
trapInteg f =
    let
        (nodes, values') = unTable f
        diffnodes = diffgrid nodes
        values  = zipWith (<+>) (tail values') values'
    in
        (0.5::t) \*\ ((laosum::[f]->f) (zipWith (\*\) diffnodes values))