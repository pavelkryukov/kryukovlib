{-
 - algorithms/hopf/lax.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Algorithms.Hopf.Lax
    (lax)
where

import Prelude hiding (Num(..))

import KryukovLib.Generic.ListFunctions (tail2)

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Number

import KryukovLib.Types.Table

import KryukovLib.Algorithms.Hopf

{-
      z
    x w y
-}
step :: (Number t) => t -> Table t t -> Table t t
step tau func = zipTable grid' result
    where
        (grid', xlist) = unTable func
        grid   = grid' - (tail2 grid')
        wlist  = tail  xlist
        ylist  = tail2 xlist
        vlist' = xlist + ylist
        vlist  = map (/2) vlist'                      -- (v = x + y / 2)
        vth    = map (* tau) $ zipWith (/) vlist grid -- (q = v * t / H)
        xylist = ylist - xlist              -- 
        u1     = zipWith (*) vth xylist               -- (x - y) * q  
        result = (head xlist) : (u1 + wlist) ++ [head xlist]  -- result
       
lax :: (Number t) => t -> Hopf t t t
lax tau function =
    zipTable (iterate (+ tau) 0) (function : iterate (step tau) function)