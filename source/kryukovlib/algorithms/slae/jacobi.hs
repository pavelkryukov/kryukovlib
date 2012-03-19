{-
 - algorithms/slae/jacobi.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE FlexibleContexts #-}
module KryukovLib.Algorithms.SLAE.Jacobi
    (jacobi)
where

import KryukovLib.Generic.Peano

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Semigroup
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Vector (Vector(..), mapVV)
import KryukovLib.Types.Matrix (SqrMatrix, SLAE(..), diag, takeDiag)

import KryukovLib.Common.Convergentor (matrixiterator)

import KryukovLib.Algorithms.SLAE

-- |Inverse matrix of diagonal part of specified matrix
adiag :: (Peano s, Fractional t, Semigroup t) => SqrMatrix s t -> SqrMatrix s t
adiag = diag . (mapVV recip) . takeDiag

-- |Jacobi iteration method
jacobi :: 
    (Peano s, 
     Semigroup t,
     LAO (Vector s t),
     LAO (SqrMatrix s t),
     Fractional t) =>
    SLAESolver s t
jacobi =
    \(SLAE a f) ->
        let
            d = adiag a
        in
            matrixiterator (iden <-> (d <*> a)) (d \*\ f) zero