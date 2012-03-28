{-
 - algorithms/integ/rectangles.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.Integ.Rectangles
    (rectInteg)
where

import KryukovLib.Generic.ListFunctions (evenl, oddl, diffgrid)

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Number
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Table (unTable, qual)

import KryukovLib.Algorithms.Integ

-- |Rectangles integral method
-- Table function should have odd number of points
rectInteg :: (CrossMult t f f, Number t, LAO f) => Integ t f
rectInteg f
    | (((mod) (qual f) 2) == 1) = error "Table should have odd number of points"
    | otherwise =
        let
            (nodes, values') = unTable f
            diffnodes = diffgrid $ evenl nodes
            values = oddl values'
        in
        laosum $ zipWith (\*\) diffnodes values