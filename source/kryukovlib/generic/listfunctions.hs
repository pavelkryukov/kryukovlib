{-
 - generic/listfunctions.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Generic.ListFunctions
    (merge, change, infstep, hgrid, chebPolyZeroes,
     evenl, oddl, diffgrid, shift, midgrid, superzip)
where

-- Merge of two lists-- [a,a,a,a,a] [b,b,b,b,b] -> [a,b,a,b,a,b,a,b]
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- Change of n-th element of list
change :: [t] -> Int -> t -> [t]
change v 0 h = h:(tail v)
change v n h = (head v):(change v (n-1) h)

evenl :: [t] -> [t]
evenl (x:xs) = x : (oddl xs)
evenl [] = []

oddl :: [t] -> [t]
oddl (_:xs) = evenl xs
oddl [] = []

-- Infinite even list with specified start and step value
infstep :: (Num t) => t -> t -> [t]
infstep a h0 = a : (infstep (a + h0) h0)

-- Take differences between list elements
diffgrid :: (Num t) => [t] -> [t]
diffgrid a = zipWith (-) (tail a) a

-- Even grid from a to b
hgrid :: (Fractional t) => (t, t) -> Int -> [t]
hgrid (a,b) n =
    let
        h = ((b - a)/(fromIntegral (n-1)))
    in
        take n (infstep a h)

shift :: [t] -> [t]
shift a = (head a) : a

midgrid :: (Fractional t) => [t] -> [t]
midgrid a = map (/2) (zipWith (+) (tail a) a)

superzip :: (t -> t -> t) -> [[t]] -> [t]
superzip _ []     = undefined
superzip _ [a]    = a
superzip f (a:bs) = zipWith f a (superzip f bs)

-- Zeroes of n-th Chebyshev polynom
chebPolyZeroes :: (Floating t) => Integer -> [t]
chebPolyZeroes n =
    map
        (\i -> cos(((2*(fromInteger i) + 1) / (2 * (fromInteger n) + 1)) * pi))
        [0..n]