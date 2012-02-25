{-
 - types/complex.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE IncoherentInstances #-}
module KryukovLib.Types.Complex
    (Complex(), i, arg, Quaternion, Octonion)
where

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Semigroup

-- Complex number type
-- First value is Re, second is Im
data Complex t    = Complex t t
type Quaternion t = Complex (Complex t)
type Octonion t   = Complex (Quaternion t)

-- Imaginary unit
i :: (Semigroup t) => Complex t
i = Complex zero iden

-- Argument
arg :: (Floating t) => (Complex t) -> t
arg (Complex x y) = atan (y / x)

-- Eq
instance (Eq t) => Eq (Complex t) where
    Complex x1 y1 == Complex x2 y2 = (x1 == x2) && (y1 == y2)

-- Num
instance (Num t) => Num (Complex t) where
    Complex a1 b1 + Complex a2 b2 = Complex (a1 + a2) (b1 + b2)
    negate (Complex x y) = Complex (negate x) (negate y)
    Complex a1 b1 * Complex a2 b2 = 
        Complex (a1 * a2 - b1 * b2) (a1 * b2 + a2 * b1)
    abs _ = undefined
    signum _ = undefined
    fromInteger x = Complex (fromInteger x) 0

instance (Fractional t) => Fractional (Complex t) where
    fromRational c = Complex (fromRational c) 0
    recip (Complex x y) = Complex (x / (x*x + y*y)) (-y / (x*x + y*y))

instance (Semigroup t, Floating t) => Floating (Complex t) where
    pi = Complex pi 0
    exp (Complex x y) = Complex (exp x * cos y) (exp x * sin y)
    sin (Complex x y) = Complex (sin x * cosh y) (cos x * sinh y)
    cos (Complex x y) = Complex (cos x * cosh y) (- sin x * sinh y)
    log (Complex x y) = Complex ((log (x * x + y * y)) / 2) (atan (y / x))
    sinh z = -i * sin (i * z)
    cosh z = cos (i * z)
    asin z = - i * log (i * z + sqrt (1 - z * z))
    acos z = pi / 2 - asin z
    atan z = (i / 2) * (log (1 - i * z) - log (1 + i * z))
    asinh z = log (z + sqrt (z * z + 1))
    acosh z = log (z + sqrt (z + 1) * sqrt (z - 1))
    atanh z = (log ((1 + z) / (1 - z))) / 2

instance (Show t) => Show (Complex t) where
    show (Complex x y) = "{" ++ (show x) ++ " + i" ++ (show y) ++ "}"

instance (LAO t) => LAO (Complex t) where
    zero = Complex zero zero
    norm1 = sqrt . euclid
    norm2 = sqrt . euclid
    euclid (Complex x y) = (euclid x) + (euclid y)
    Complex a1 b1 <+> Complex a2 b2 = Complex (a1 <+> a2) (b1 <+> b2)
    Complex a1 b1 <-> Complex a2 b2 = Complex (a1 <-> a2) (b1 <-> b2)
    
instance (Semigroup t) => Semigroup (Complex t) where
    iden = Complex iden zero
    Complex a1 b1 <*> Complex a2 b2 =
        Complex ((a1 <*> a2) <-> (b1 <*> b2)) ((a1 <*> b2) <+> (a2 <*> b1))