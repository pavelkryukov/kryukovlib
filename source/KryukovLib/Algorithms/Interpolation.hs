{-
 - algorithms/interpolation.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.Interpolation
    (Interpolation)
where

import Prelude hiding (Num(..))

import KryukovLib.Types.Table (Table)

-- |Interpolation operator converts table function to analytic function
-- Table function should be sorted
type Interpolation t f = Table t f -> (t -> f)
