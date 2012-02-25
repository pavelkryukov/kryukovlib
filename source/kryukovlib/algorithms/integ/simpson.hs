{-
 - algorithms/integr/simpson.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE ScopedTypeVariables #-}
module KryukovLib.Algorithms.Integ.Simpson
    (simpson)
where

import KryukovLib.Generic.ListFunctions 
    (evenl, oddl, diffgrid, superzip)

import KryukovLib.Classes.CrossMult
import KryukovLib.Classes.LAO

import KryukovLib.Types.Table (unTable, qual)

import KryukovLib.Algorithms.Integ

-- Intergal of table function
simpson :: forall t f.
    (CrossMult t f f, Fractional t, LAO f) => Integ t f
simpson func 
    | (((mod) (qual func) 2) == 1) = error "Table should have 2*N+1 points"
    | otherwise =
        let
            (nodes, values') = unTable func
            diffnodes = diffgrid $ evenl nodes
            values = 
                superzip (<+>) [
                    evenl values',
                    map ((4::t) \*\) (oddl values'),
                    tail (evenl values')
                ]
        in
        (1/6::t) \*\ (laosum::[f]->f) (zipWith (\*\) diffnodes values)