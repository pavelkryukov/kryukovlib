{-
 - algorithms/interpolation/newton.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE MonoLocalBinds #-}
module KryukovLib.Algorithms.Interpolation.Newton
    (newton)
where

import Prelude hiding (Num(..))

import KryukovLib.Classes.LAO
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Table (qual, unTable)

import KryukovLib.Common.FinDiff (finDiff)

import KryukovLib.Algorithms.Interpolation

-- |Interpolation polynom in Newton's form
newton :: (NumberMult t f) => Interpolation t f
newton f =
    \x ->
        laosum $ zipWith (\*\) (map ($ x) poly) (finDiff f)
    where
        (nodes, _) = unTable f
        q = qual f
        poly =
            map (\n -> \x -> product (map (x -) (take n nodes))) [0..q]