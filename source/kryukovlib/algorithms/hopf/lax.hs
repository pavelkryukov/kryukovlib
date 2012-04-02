{-
 - algorithms/hopf/lax.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.Hopf.Lax
    (lax)
where

import KryukovLib.Classes.Number

import KryukovLib.Types.Table

import KryukovLib.Algorithms.Hopf

{-
      z
    x w y
-}

tail2 :: [t] -> [t]
tail2 = tail . tail

step :: (Number t) => t -> Table t t -> Table t t
step tau func = zipTable grid' result
    where
        (grid', xlist) = unTable func
        grid   = zipWith (-) grid' (tail2 grid')
        wlist  = tail  xlist
        ylist  = tail2 xlist
        vlist' = zipWith (+) xlist ylist
        vlist  = map (/2) vlist'                      -- (v = x + y / 2)
        vth    = map (* tau) $ zipWith (/) vlist grid -- (q = v * t / H)
        xylist = zipWith (-) ylist xlist              -- 
        u1     = zipWith (*) vth xylist               -- (x - y) * q  
        result = (head xlist) : (zipWith (+) u1 wlist) ++ [head xlist]  -- result
       
lax :: (Number t) => t -> NLAdvection t t t
lax tau function =
    zipTable (iterate (+ tau) 0) (function : iterate (step tau) function)