{-
 - algorithms/matrixinv/slaeinverse.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE FlexibleContexts #-}
module KryukovLib.Algorithms.MatrixInv.SLAEInverse
    (slaeInverse)
where

import Data.Maybe

import KryukovLib.Generic.Peano

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Semigroup

import KryukovLib.Types.Vector (Vector, basis)
import KryukovLib.Types.Matrix (Matrix(..), trans, SLAE(..))

import KryukovLib.Algorithms.SLAE (SLAESolver)

import KryukovLib.Algorithms.MatrixInv

-- |Matrix invertor with using SLAESolver
-- Counts every line of inverse matrix as solution of
-- equation with basis column on the right
slaeInverse :: (Peano s, LAO (Vector s t), Semigroup t) =>
    (SLAESolver s t) -> (MatrixInvertor s s t)
slaeInverse solver =
    \a ->
        case mapMaybe (solver . (SLAE a)) basis of
            [] -> Nothing
            f -> Just $ trans (Matrix f)