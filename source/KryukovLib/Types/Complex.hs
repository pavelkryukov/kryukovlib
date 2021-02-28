{-
 - types/complex.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
{-# LANGUAGE IncoherentInstances, MonoLocalBinds #-}
module KryukovLib.Types.Complex
    (Complex(), i, arg, Quaternion, Octonion)
where

import Prelude hiding (Num(..), Semigroup)
import qualified Prelude as P

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Number
import KryukovLib.Classes.Semigroup

-- |Complex number type
-- First value is Re, second is Im
data Complex t    = Complex t t

-- |Quaternion synonym for Complex on Complex
type Quaternion t = Complex (Complex t)

-- |Octonion synonym for Complex on Complex
type Octonion t   = Complex (Quaternion t)

-- |Imaginary unit
i :: (Semigroup t) => Complex t
i = Complex zero iden

two :: (Semigroup t) => Complex t
two = iden + iden

-- |Argument
arg :: (Floating t) => (Complex t) -> t
arg (Complex x y) = atan (y / x)

instance (Eq t) => Eq (Complex t) where
    Complex x1 y1 == Complex x2 y2 = (x1 == x2) && (y1 == y2)

instance (P.Num t) => P.Num (Complex t) where
    Complex a1 b1 + Complex a2 b2 = Complex (a1 P.+ a2) (b1 P.+ b2)
    negate (Complex x y) = Complex (P.negate x) (P.negate y)
    Complex a1 b1 * Complex a2 b2 = 
        Complex (a1 P.* a2 P.- b1 P.* b2) (a1 P.* b2 P.+ a2 P.* b1)
    abs _ = undefined
    signum _ = undefined
    fromInteger x = Complex (P.fromInteger x) 0

instance (P.Num t, Number t, Fractional t) => Fractional (Complex t) where
    fromRational c = Complex (fromRational c) zero
    recip (Complex x y) = Complex (x / (x*x + y*y)) (-y / (x*x + y*y))

instance (Number t, Semigroup t, Floating t) => Floating (Complex t) where
    pi = Complex pi 0
    exp (Complex x y) = Complex (exp x * cos y) (exp x * sin y)
    sin (Complex x y) = Complex (sin x * cosh y) (cos x * sinh y)
    cos (Complex x y) = Complex (cos x * cosh y) (- sin x * sinh y)
    log (Complex x y) = Complex ((log (x * x + y * y)) / 2) (atan (y / x))
    sinh z = zero - i * sin (i * z)
    cosh z = cos (i * z)
    asin z = zero - i * log (i * z + sqrt (iden - z * z))
    acos z = pi / two - asin z
    atan z = (i / (iden + iden)) * (log (iden - i * z) - log (iden + i * z))
    asinh z = log (z + sqrt (z * z + iden))
    acosh z = log (z + sqrt (z + iden) * sqrt (z - iden))
    atanh z = (log ((iden + z) / (iden - z))) / two
    
instance (Show t) => Show (Complex t) where
    show (Complex x y) = "{" ++ (show x) ++ " + i" ++ (show y) ++ "}"
   
instance (LAO t) => LAO (Complex t) where
    zero = Complex zero zero
    norm1 = sqrt . euclid
    norm2 = sqrt . euclid
    euclid (Complex x y) = (euclid x) + (euclid y)
    Complex a1 b1 + Complex a2 b2 = Complex (a1 + a2) (b1 + b2)
    Complex a1 b1 - Complex a2 b2 = Complex (a1 - a2) (b1 - b2)
    
instance (Semigroup t) => Semigroup (Complex t) where
    iden = Complex iden zero
    Complex a1 b1 * Complex a2 b2 =
        Complex ((a1 * a2) - (b1 * b2)) ((a1 * b2) + (a2 * b1))
