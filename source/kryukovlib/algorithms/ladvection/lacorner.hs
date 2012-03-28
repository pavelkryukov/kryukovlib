{-
 - algorithms/ladvection/lacorner.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE TypeFamilies #-}
module KryukovLib.Algorithms.LAdvection.LACorner
    (lacorner)
where

import KryukovLib.Generic.ListFunctions (diffgrid)

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Number
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Table (Table, unTable, zipTable)

import KryukovLib.Algorithms.LAdvection (LAdvection)

{-
      z
    x y
-}
step :: (t ~ x, LAO f, Number t, CrossMult t f f) =>
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
        result = v0 : (zipWith (<+>) u1 u2)  -- result
       
lacorner :: (LAO f, Number t, CrossMult t f f) => LAdvection t f
lacorner funcX funcT = zipTable tnodes result
    where
        (tnodes, tvalues) = unTable funcT
        taus = diffgrid tnodes
        result = funcX : (zipWith3 step (tail tvalues) taus result)