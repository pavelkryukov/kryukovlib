{-
 - algorithms/matrixinv.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.MatrixInv
    (MatrixInvertor)
where

import Prelude hiding (Num(..))

import KryukovLib.Types.Matrix (Matrix)

-- |Matrix invertor function type
type MatrixInvertor s1 s2 t = Matrix s1 s2 t -> Maybe (Matrix s2 s1 t)
