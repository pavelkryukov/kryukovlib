{-
 - algorithms/ladvection/laimplicitcorner.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE TypeFamilies #-}
module KryukovLib.Algorithms.LAdvection.LAImplicitCorner
    (laimplicitcorner)
where

import KryukovLib.Generic.ListFunctions (diffgrid)

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Number
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Table (Table, unTable, zipTable)

import KryukovLib.Algorithms.LAdvection (LAdvection)

{-
    y z
      x 
-}

step :: (t ~ x, LAO f, Number t, CrossMult t f f) =>
    f -> t -> Table x f -> Table x f
step v0 tau func = zipTable grid' result
    where
        (grid', values) = unTable func
        th      = map (* tau) (diffgrid grid')        -- q = t / h
        u1      = zipWith (\*\) th result             -- y * q
        u21     = zipWith (<+>) u1 (tail values)      -- y * q + x
        uall    = zipWith 
                    (\b -> ((recip (1 + b)) \*\)) 
                    th
                    u21                               -- (y * q + x) / (1 + q)
        result  = v0 : uall
       
laimplicitcorner :: (LAO f, Number t, CrossMult t f f) => LAdvection t f
laimplicitcorner funcX funcT = zipTable tnodes result
    where
        (tnodes, tvalues) = unTable funcT
        taus = diffgrid tnodes
        result = funcX : (zipWith3 step (tail tvalues) taus result)