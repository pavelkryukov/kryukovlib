{-
 - algorithms/derivative.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.Derivative
    (Derivate)
where

import Prelude hiding (Num(..))

-- |Derivate operator converts
-- analytic function to analytic function of same type
type Derivate t f = (t -> f) -> (t -> f)