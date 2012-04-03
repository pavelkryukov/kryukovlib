{-
 - classes/lao.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE 
    FlexibleInstances,
    UndecidableInstances,
    OverlappingInstances,
    IncoherentInstances #-}
module KryukovLib.Classes.LAO
    (LAO(..), Norm)
where

import Prelude hiding (Num(..))
import qualified Prelude as P

import KryukovLib.Classes.Number

type Norm a = a -> Double

-- |Class for Linear Algebra Object
-- with defined +, -, 0 and norms
class LAO a where
    -- Zero element
    zero :: a
    -- Norms
    norm1 :: Norm a
    norm2 :: Norm a
    euclid :: Norm a    
    norm3 :: Norm a
    -- Linear operations
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    -- Linear list sum
    laosum :: [a] -> a
    laosum = foldl (+) zero
    norm3 = sqrt . euclid
    
instance (Number a) => LAO a where
    zero = 0
    norm1 = P.abs . fromPrecise
    norm2 = P.abs . fromPrecise
    euclid = (**2) . fromPrecise
    (+) = (P.+)
    (-) = (P.-)

instance (LAO a) => LAO [a] where
    zero = repeat zero
    norm1  = maximum . (map norm1)
    norm2  = sum . (map norm2)
    euclid = sum . map euclid
    a + b = zipWith (+) a b
    a - b = zipWith (-) a b