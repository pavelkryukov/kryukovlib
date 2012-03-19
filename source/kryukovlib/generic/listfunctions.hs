{-
 - generic/listfunctions.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Generic.ListFunctions
    (merge, hgrid, chebPolyZeroes,
     evenl, oddl, diffgrid, shift, midgrid, superzip)
where

-- |Merge of two lists-- [a,a,a,a,a] [b,b,b,b,b] -> [a,b,a,b,a,b,a,b]
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- |Even elements of list
evenl :: [t] -> [t]
evenl (x:xs) = x : (oddl xs)
evenl [] = []

-- |Odd elements of list
oddl :: [t] -> [t]
oddl (_:xs) = evenl xs
oddl [] = []

-- |Take differences between list elements
diffgrid :: (Num t) => [t] -> [t]
diffgrid a = zipWith (-) (tail a) a

-- |Even grid from a to b
hgrid :: (Fractional t) => (t, t) -> Int -> [t]
hgrid (a,b) n =
    let
        h = ((b - a)/(fromIntegral (n-1)))
    in
        take n (iterate (+ h) a)

-- |Shift of list with copying of first element
shift :: [t] -> [t]
shift a = (head a) : a

-- |Returns lists of pair mediums of list elements
midgrid :: (Fractional t) => [t] -> [t]
midgrid a = map (/2) (zipWith (+) (tail a) a)

-- |Zips list of lists into list with selected function
superzip :: (t -> t -> t) -> [[t]] -> [t]
superzip _ []     = undefined
superzip _ [a]    = a
superzip f (a:bs) = zipWith f a (superzip f bs)

-- |Zeroes of n-th Chebyshev polynom
chebPolyZeroes :: (Floating t) => Integer -> [t]
chebPolyZeroes n =
    map
        (\i -> cos(((2*(fromInteger i) + 1) / (2 * (fromInteger n) + 1)) * pi))
        [0..n]