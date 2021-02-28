{-
 - algorithms/derivative/onesided.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE ScopedTypeVariables, MonoLocalBinds #-}
module KryukovLib.Algorithms.Derivative.OneSided
    (deriv)
where

import Prelude hiding (Num(..))

import KryukovLib.Generic.Debug (notImpl)

import KryukovLib.Classes.LAO
import KryukovLib.Classes.CrossMult

import KryukovLib.Algorithms.Derivative

-- |Onesided derivative
deriv :: forall t f. 
    (NumberMult t f) =>
    Int -> t -> Derivate t f
deriv n h =
    \func ->
    \x ->
        (recip h) \*\
        (laosum::[f]->f) (zipWith (\*\) (coeffs n) (map func (iterate (+ h) x)))
        where
            coeffs :: Int -> [t]
            coeffs 1 = [-1, 1]
            coeffs 2 = [-3/2, 2, -1/2]
            coeffs 3 = [-11/6, 3, -3/2, 1/3]
            coeffs 4 = [-25/12, 4, -3, 4/3, -1/4]
            coeffs _ = notImpl