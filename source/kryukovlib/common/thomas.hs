{-
 - common/thomas.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Common.Thomas
    (thomas)
where

import KryukovLib.Generic.Peano

import KryukovLib.Types.Vector (Vector(..))

-- Solver of 3-diagonal SLAE
thomas :: (Peano s, Fractional t) =>
    Vector s t -> Vector s t -> Vector s t -> Vector s t -> Vector s t
thomas (Vector a) (Vector b) (Vector c) (Vector f) = Vector (reverse colX)
    where
        denom = zipWith (-) b (zipWith (*) (0:colP) a)
        colP  = zipWith (/) c denom
        nomQ  = zipWith (-) f (zipWith (*) (0:colQ) a)
        colQ  = zipWith (/) nomQ denom
        colX  = zipWith (-) (reverse colQ) (zipWith (*) (reverse colP) (0:colX))