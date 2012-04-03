{-
 - algorithms/hopf/hopfcorner.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.Hopf.HopfCorner
    (hopfcorner)
where

import Prelude hiding (Num(..))

import KryukovLib.Generic.ListFunctions

import KryukovLib.Classes.Number

import KryukovLib.Types.Table

import KryukovLib.Algorithms.Hopf

{-
    y z
      x 
-}
step :: (Number t) => t -> Table t t -> Table t t
step tau func = zipTable grid' result'
    where
        (grid', values) = unTable func
        grid    = diffgrid grid'
-- prediction
        vlist   = map (/2) (zipWith (+) result (tail values)) -- v = (y + x) / 2
        vth     = map (* tau) $ zipWith (/) vlist grid        -- q = v * t / h
        u1      = zipWith (*) vth result                      -- y * q
        u21     = zipWith (+) u1 (tail values)                -- y * q + x
        uall    = 
            zipWith 
                (\a -> \b -> a / (1 + b)) 
                u21 
                vth                                     -- (y * q + x) / (1 + q)
        result  = (head values) : uall
-- correction
        vlist'  = midgrid result                              -- v = (y + z) / 2
        vth'    = map (* tau) $ zipWith (/) vlist' grid       -- q = v * t / h
        u1'     = zipWith (*) vth' result'                    -- y * q
        u21'    = zipWith (+) u1' (tail values)               -- y * q + x
        uall'   =
            zipWith
                (\a -> \b -> a / (1 + b))
                u21'
                vth'                                    -- (y * q + x) / (1 + q)
        result' = (head values) : uall'
       
hopfcorner :: (Number t) => t -> Hopf t t t
hopfcorner tau function =
    zipTable (iterate (+ tau) 0) (function : iterate (step tau) function)