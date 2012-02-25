{-
 - types/table.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Types.Table
    (Table, zipTable, inverseT, trace, qual, unTable)
where

import Data.Tuple

-- Table saves values (x,y)
data Table t f = Table [(t,f)] Int

instance (Show t, Show f) => Show (Table t f) where
    show (Table a _) = 
        let 
            showpoint (x, y) = (show x) ++ " -> " ++ (show y)
        in
            concat $ map ((++ "\n") . showpoint) a

zipTable :: [t] -> [f] -> Table t f
zipTable x y = Table (zip x y) (length x - 1)
            
unTable :: Table t f -> ([t], [f])
unTable (Table func _) = unzip func

-- Inverse of table function
inverseT :: Table t f -> Table f t
inverseT (Table f q) = Table (map swap f) q

-- Generator of table functions from range and analytic function --
trace :: [t] -> (t -> f) -> Table t f
trace grid func = Table (map (\i -> (i, func i)) grid) (length grid - 1)

-- Quality of table function (amount of points - 1)
qual :: Table t f -> Int
qual (Table _ q) = q