{-
 - algorithms/slae/seidel.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE FlexibleContexts, MonoLocalBinds #-}
module KryukovLib.Algorithms.SLAE.Seidel
    (seidel)
where

import Prelude hiding (Num(..), Semigroup)

import Data.Maybe (fromJust)

import KryukovLib.Generic.Peano

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Number
import KryukovLib.Classes.Semigroup
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Vector (Vector(..))
import KryukovLib.Types.Matrix (SqrMatrix, SLAE(..), lmatrix, umatrix)

import KryukovLib.Common.Convergentor (matrixiterator)

import KryukovLib.Algorithms.SLAE.Gauss (gauss)
import KryukovLib.Algorithms.MatrixInv.SLAEInverse

import KryukovLib.Algorithms.SLAE

-- |Seidel iteration method
seidel :: 
    (Peano s,
     Number t,
     Semigroup t,
     LAO (Vector s t),
     LAO (SqrMatrix s t)) =>
    SLAESolver s t
seidel =
    \(SLAE a f) ->
        let
            (l, u) = (lmatrix a, umatrix a)
            u' = fromJust ((slaeInverse gauss) u)
            b = zero - (u' * l)
        in
            matrixiterator b (u' \*\ f) zero