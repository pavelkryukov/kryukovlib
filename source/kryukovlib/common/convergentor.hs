{-
 - common/convergentor.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Common.Convergentor
    (convergentor, matrixiterator)
where

import KryukovLib.Classes.LAO
import KryukovLib.Classes.CrossMult
import KryukovLib.Classes.Semigroup

import KryukovLib.Types.Matrix (SqrMatrix)
import KryukovLib.Types.Vector (Vector(..))

-- Convergentor counts value
-- u' = f(u) while u' /= u with selected quality q
-- if number of iterations is greater than n,
-- non-convergence is detected
convergentor :: (LAO a) => (a -> a) -> a -> Maybe a
convergentor = convergentor' 0

q :: Double
q = 0.000001

convergentor' :: (LAO a) => Int -> (a -> a) -> a -> Maybe a
convergentor' n func val
    | n > 100000 = Nothing
    | (norm2 (val <-> func val)) < q = Just val
    | otherwise = convergentor' (n + 1) func (func val)
    

-- Solution of SLAE using iteration form
-- x' = Bx + f
matrixiterator :: (LAO (Vector s t), Semigroup t) =>
    SqrMatrix s t -> Vector s t -> Vector s t -> Maybe (Vector s t)
matrixiterator b f = convergentor ((f <+> ) . (b \*\))