{-
 - algorithms/slae/parameter.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.SLAE.Parameter
    (parameter)
where

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Semigroup
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Vector (Vector)
import KryukovLib.Types.Matrix (SqrMatrix, SLAE(..))

import KryukovLib.Common.Convergentor (matrixiterator)

import KryukovLib.Algorithms.SLAE

-- One-parameter iteration method
-- x' = (E - tA)x + tf
parameter :: 
    (Semigroup t, LAO (Vector s t), LAO (SqrMatrix s t)) =>
        t -> SLAESolver s t
parameter tau =
    \(SLAE a f) -> matrixiterator (iden <-> (tau \*\ a)) (tau \*\ f) zero