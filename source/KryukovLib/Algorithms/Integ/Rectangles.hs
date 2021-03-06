{-
 - algorithms/integ/rectangles.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE MonoLocalBinds #-}
module KryukovLib.Algorithms.Integ.Rectangles
    (rectInteg)
where

import Prelude hiding (Num(..))

import KryukovLib.Generic.ListFunctions (evenl, oddl, diffgrid)

import KryukovLib.Classes.LAO
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Table (unTable, qual)

import KryukovLib.Algorithms.Integ

-- |Rectangles integral method
-- Table function should have odd number of points
rectInteg :: (NumberMult t f) => Integ t f
rectInteg f
    | (((mod) (qual f) 2) == 1) = error "Table should have odd number of points"
    | otherwise =
        let
            (nodes, values') = unTable f
            diffnodes = diffgrid $ evenl nodes
            values = oddl values'
        in
        laosum $ zipWith (\*\) diffnodes values