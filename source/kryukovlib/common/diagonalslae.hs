{-
 - common/diagonalslae.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Common.DiagonalSLAE
    (solve3)
where

import KryukovLib.Generic.Peano

import KryukovLib.Types.Vector (Vector(..), vtl)

-- Solver of 3-diagonal SLAE
solve3 :: (Peano s, Fractional t) =>
    Vector s t -> Vector s t -> Vector s t -> Vector s t -> Vector s t
solve3 a b c f = Vector (solve3' (vtl a) (vtl b) (vtl c) (vtl f))

solve3' :: (Fractional t) => [t]->[t]->[t]->[t]->[t]
solve3' a b c f = reverse colX
    where
        colP = zipWith (/)
            c
            (zipWith (-) b (zipWith (*) (0:colP) a))
        colQ = zipWith (/)
            (zipWith (-) f (zipWith (*) (0:colQ) a))
            (zipWith (-) b (zipWith (*) (0:colP) a))
        colX = zipWith (-) (reverse colQ) (zipWith (*) (reverse colP) (0:colX))