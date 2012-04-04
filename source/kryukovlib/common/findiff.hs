{-
 - common/findiff.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Common.FinDiff
    (finDiff)
where

import Prelude hiding (Num(..))
import qualified Prelude as P

import KryukovLib.Classes.LAO
import KryukovLib.Classes.CrossMult

import KryukovLib.Types.Table (Table, unTable, qual)

finDiff' :: (NumberMult t f) =>  
    (Table t f) -> Int -> f
finDiff' func n =
    let
        (nodes', values') = unTable func
        nodes  = take (n P.+ 1) nodes'
        values = take (n P.+ 1) values'
        denom val = product $ map (val -) (filter (/= (val)) nodes)
    in
        laosum $ zipWith (\*\) (map (recip . denom) nodes) values

-- |Finite differences from first element to last
finDiff :: (NumberMult t f) =>  
    (Table t f) -> [f] 
finDiff func = map (finDiff' func) [0..(qual func)]