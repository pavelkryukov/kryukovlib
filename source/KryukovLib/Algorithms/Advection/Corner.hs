{-
 - algorithms/advection/corner.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE TypeFamilies #-}
module KryukovLib.Algorithms.Advection.Corner
    (corner)
where

import Prelude hiding (Num(..))

import KryukovLib.Generic.ListFunctions (diffgrid)

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Number
import KryukovLib.Classes.CrossMult
import KryukovLib.Classes.Semigroup

import KryukovLib.Types.Table (Table, unTable, zipTable)

import KryukovLib.Algorithms.Advection (Advection)

{-
      z
    x y
-}
step :: (t ~ x, NumberMult t f) =>
    f -> t -> Table x f -> Table x f
step v0 tau func = zipTable grid' result
    where
        (grid', values) = unTable func
        th    = map (* tau) (diffgrid grid') -- q = t / h
        u1    = zipWith (\*\) th values      -- x * q
        u2    = zipWith 
            (\a -> \b -> (1 - a) \*\ b)
            th 
            (tail values)                    -- y * (1 - q)
        result = v0 : (zipWith (+) u1 u2)  -- result
       
corner :: (NumberMult t f) => Advection t f
corner funcX funcT = zipTable tnodes result
    where
        (tnodes, tvalues) = unTable funcT
        taus = diffgrid tnodes
        result = funcX : (zipWith3 step (tail tvalues) taus result)