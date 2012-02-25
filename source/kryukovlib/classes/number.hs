{-
 - classes/number.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Classes.Number
    (Number(..))
where

import GHC.Float (float2Double, double2Float)

-- Numbers class
-- When you convert number using function toPrecise, you can
-- use most of the Algorithms of KryukovLib
class (Num a) => Number a where
    toPrecise :: Double -> a    
    fromPrecise :: a -> Double
    analytic :: (Double -> Double) -> (a -> a)

-- Instance for floating types
instance Number Double where
    toPrecise = id
    analytic = id
    fromPrecise = id
    
-- Instance for floating types
instance Number Float where
    toPrecise = double2Float
    analytic f = double2Float . f . float2Double
    fromPrecise = float2Double