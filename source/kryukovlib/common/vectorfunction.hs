{-
 - types/vector.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Types.VectorFunction
    (splitX)
where

import KryukovLib.Types.Vector

-- Breaks function (y0, y1, y[2], ...) = f(x[0], x[1], x[2], ...)
-- to array of functions (y0, y1, y2, ...) = g(x[n])
-- in specified point
splitX :: ((Vector t) -> (Vector f)) -> Vector t -> [(t -> (Vector f))]
splitX func (Vector v0) =
    map (\n -> func . Vector . (change v0 n)) [0..(length v0) - 1]

