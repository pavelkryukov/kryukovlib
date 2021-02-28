{-
 - algorithms/derivative/symmetric.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE ScopedTypeVariables #-}
module KryukovLib.Algorithms.Derivative.Symmetric
    (derivS)
where

import Prelude hiding (Num(..))

import KryukovLib.Generic.Debug (notImpl)
import KryukovLib.Generic.ListFunctions (merge)

import KryukovLib.Classes.LAO
import KryukovLib.Classes.CrossMult

import KryukovLib.Algorithms.Derivative

-- |Symmetric derivative
derivS :: forall t f. 
    (NumberMult t f) =>
        Int -> t -> Derivate t f
derivS n h =
    \func ->
    \x ->
        (recip h) \*\
        (laosum::[f]->f) (zipWith (\*\) (coeffs n) (map func (gridS x)))
        where
            gridS a = merge ((iterate (+ (0 - h))) (a - h)) (iterate (+ h) (a + h))
            coeffs :: Int -> [t]
            coeffs 2 = [-1/2, 1/2]
            coeffs 4 = [4/3, -4/3, -1/12, 1/12]
            coeffs _ = notImpl
