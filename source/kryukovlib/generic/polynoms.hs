{-
 - generic/polynoms.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Generic.Polynoms
    (legePoly, chebPoly, normedChebPoly, reducedChebPoly)
where

-- |Legendre polynoms
legePoly :: (Fractional t) => Int -> (t -> t)
legePoly 0 = const 1
legePoly 1 = id
legePoly n = 
    \x ->
    let
        n' = fromIntegral n
    in
        ((2 * n' - 1) / n') * x * (legePoly (n - 1) x) - 
        (n' - 1) / n' * (legePoly (n - 2) x)

chebPoly' :: (Fractional t, Floating t) => Int -> (t -> t)
chebPoly' 0 = const 1
chebPoly' 1 = id
chebPoly' n = \x -> 2 * x * (chebPoly' (n - 1) x) - (chebPoly' (n - 2) x)

-- |Chebyshev polynoms
chebPoly  :: (Fractional t, Floating t) => [t -> t]
chebPoly = map chebPoly' [0..]

-- |Normed Chebyshev polynoms
normedChebPoly :: (Fractional t, Floating t) => [t -> t]
normedChebPoly = map (\n -> ((/(2^(n-1))) . chebPoly' n)) [0..]

-- |Reduced Chebyshev polynoms
reducedChebPoly :: (Fractional t, Floating t) => [(t -> t)]
reducedChebPoly = (\_ -> 1 / sqrt(2)) : (tail chebPoly)