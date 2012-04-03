{-
 - algorithms/ladvection.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.LAdvection
    (LAdvection)
where

import Prelude hiding (Num(..))

import KryukovLib.Types.Table (Table)

-- |LAdvection solves equation du/dt + du/dx = 0
type LAdvection t f = Table t f -> Table t f -> Table t (Table t f)
