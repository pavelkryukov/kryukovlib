{-
 - algorithms/nladvection/nlacorner.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.NLAdvection.NLACorner
    (nlacorner)
where

import KryukovLib.Generic.ListFunctions

import KryukovLib.Types.Table

import KryukovLib.Algorithms.NLAdvection

{-
      z
    x y
-}

step :: (Fractional t) => t -> Table t t -> Table t t
step tau func = zipTable grid' result
    where
        (grid', values) = unTable func
        grid   = diffgrid grid'        
        vlist  = midgrid values                       -- (v = x + y / 2)
        vth    = map (* tau) $ zipWith (/) vlist grid -- (q = v * t / h)
        u1     = zipWith (*) vth values               -- x * q  
        u2     = zipWith 
            (\a -> \b -> b * (1 - a)) 
            vth 
            (tail values)                             -- y * (1 - q)
        result = (head values) : (zipWith (+) u1 u2)  -- result
       
nlacorner :: (Fractional t) => t -> Advection t t t
nlacorner tau function =
    zipTable (iterate (+ tau) 0) (function : iterate (step tau) function)