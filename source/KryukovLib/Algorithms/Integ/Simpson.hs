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

import Prelude hiding (Num(..))

import KryukovLib.Generic.ListFunctions 
    (evenl, oddl, diffgrid, superzip)

import KryukovLib.Classes.CrossMult
import KryukovLib.Classes.LAO

import KryukovLib.Types.Table (unTable, qual)

import KryukovLib.Algorithms.Integ

-- |Simpson integral method
-- Table function should have odd number of points
simpson :: forall t f.
    (NumberMult t f) => Integ t f
simpson fc
    | (((mod) (qual fc) 2) == 1) = error "Table should have odd number of points"
    | otherwise =
        let
            (nodes, values') = unTable fc
            diffnodes = diffgrid $ evenl nodes
            values = 
                superzip (+) [
                    evenl values',
                    map ((4::t) \*\) (oddl values'),
                    tail (evenl values')
                ]
        in
        (1/6::t) \*\ (laosum::[f]->f) (zipWith (\*\) diffnodes values)