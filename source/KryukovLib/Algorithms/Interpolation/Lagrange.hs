{-
 - algorithms/interpolation/lagrange.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE MonoLocalBinds #-}
module KryukovLib.Algorithms.Interpolation.Lagrange
    (lagrange)
where

import Prelude hiding (Num(..))

import KryukovLib.Classes.LAO
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Table (unTable)

import KryukovLib.Algorithms.Interpolation

-- |Interpolation polynom in Lagrange's form --
lagrange :: (NumberMult t f) => Interpolation t f
lagrange func =
    \x -> 
        laosum $ zipWith (\*\) (map ($ x) base) values
    where
        (nodes, values) = unTable func
        base = map
            (\val -> \x -> product $ map (\y -> (x - y) / (val - y)) (filter (/= val) nodes))
             nodes