{-
 - common/findiff.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Common.FinDiff
    (finDiff)
where

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Number
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Table (Table, unTable, qual)

finDiff' :: (CrossMult t f f, LAO f, Eq t, Number t) =>  
    (Table t f) -> Int -> f
finDiff' func n =
    let
        (nodes', values') = unTable func
        nodes  = take (n + 1) nodes'
        values = take (n + 1) values'
        denom val = product $ map (val -) (filter (/= (val)) nodes)
    in
        laosum $ zipWith (\*\) (map (recip . denom) nodes) values

-- |Finite differences from first element to last
finDiff :: (CrossMult t f f, LAO f, Eq t, Number t) =>  
    (Table t f) -> [f] 
finDiff func = map (finDiff' func) [0..(qual func)]