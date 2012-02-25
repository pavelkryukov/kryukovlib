{-
 - solutions.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Solutions
    (integA, hderiv, inverseA, condNumber)
where

import KryukovLib.Generic.ListFunctions (hgrid)

import KryukovLib.Classes.LAO

import KryukovLib.Types.Table (trace, inverseT)
import KryukovLib.Types.Matrix (SqrMatrix)

import KryukovLib.Algorithms.Derivative
import KryukovLib.Algorithms.Integ
import KryukovLib.Algorithms.Interpolation
import KryukovLib.Algorithms.MatrixInv

-- Integral of analytic function
-- First argument is for integral algorithm,
-- second for interval, third is number of nodes
-- and the fourth one is function
integA  :: (Fractional t, LAO f) =>
    Integ t f -> (t,t) -> Int -> (t -> f) -> f
integA integ interval quality function =
    integ (trace (hgrid interval quality) function)

-- hderiv repeats deriving operator 
-- First argument is deriving operator
-- second is the order of deriving
hderiv :: (Derivate t f) -> Int -> (Derivate t f)
hderiv _ 0 = \func -> func
hderiv d n = (hderiv d (n - 1)) . d

--jacobian :: (Derivate t (Vector f)) ->
--    ((Vector t) -> (Vector f)) -> ((Vector t) -> (Matrix f))
--jacobian diff func =
--    \v -> Matrix (zipWith (diff) (splitX func v) (vtl v))

-- Inverter of function with help of interpolation
-- Parameters: interpolation operator
--             interval of inverse
--             number of points interval
--             function
inverseA  :: (Fractional t, LAO f) =>
    Interpolation f t -> (t,t) -> Int -> (t -> f) -> (f -> t)
inverseA interpolation interval quality function =
    interpolation
        (inverseT (trace (hgrid interval quality) function))

-- Counter of coniditional number of Matrix
-- using Matrix invertion method (first arg)
-- and specified Norm function (second arg)
condNumber :: (MatrixInvertor s s t) ->
    (Norm (SqrMatrix s t)) -> (SqrMatrix s t -> Maybe (Double))
condNumber inv norm =
    \a ->
        case inv a of
            Just inva -> Just ((norm a) * (norm inva))
            Nothing -> Nothing