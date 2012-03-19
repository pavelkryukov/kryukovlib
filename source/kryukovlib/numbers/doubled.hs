{-
 - numbers/doubled.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Numbers.DoubleD
    (DoubleD)
where

import KryukovLib.Classes.Number

import KryukovLib.Numbers.QRational

-- |DoubleD is type that allow count numbers in Floating
-- and Rational mode and count imprecisions
-- Ord class is not instanced for that type!
-- Please use Norm2 if you need to comparse numbers.
data DoubleD = DoubleD QRational Double deriving (Read, Eq)

-- |Imprecision counting
imprecision :: DoubleD -> Rational
imprecision (DoubleD a b) =
    ((toRational b) - (toRational a)) / (toRational a)

-- |Wrapper for function of one argument
f1 :: (QRational -> QRational) ->
    (Double -> Double) ->
    (DoubleD -> DoubleD)
f1 funca funcb =
    \(DoubleD a b) ->
    DoubleD (funca a) (funcb b)

-- |Wrapper for function of two arguments
f2 :: (QRational -> QRational -> QRational) ->
    (Double -> Double -> Double) ->
    (DoubleD -> DoubleD -> DoubleD)
f2 funca funcb =
    \(DoubleD a2 b2) ->
    \(DoubleD a1 b1) ->
    DoubleD (funca a1 a2) (funcb b1 b2)

-- |Arithmetics
instance Num DoubleD where
    (+) = f2 (+) (+)
    negate = f1 negate negate
    (*) = f2 (*) (*)
    abs = f1 abs abs
    signum = f1 signum signum
    fromInteger a = DoubleD (fromInteger a) (fromInteger a)

instance Fractional DoubleD where
    fromRational c = DoubleD (fromRational c) (fromRational c)
    recip = f1 recip recip

-- |Converter to precised type.
instance Number DoubleD where
    toPrecise a = DoubleD (toPrecise a) (toPrecise a)
    fromPrecise (DoubleD _ a) = a
    analytic func = \(DoubleD a b) -> DoubleD (analytic func a) (func b)

-- |Common functions
instance Floating DoubleD where
    pi = toPrecise pi
    exp = analytic exp
    log = analytic log
    sin = analytic sin
    cos = analytic cos
    asin = analytic asin
    acos = analytic acos
    atan = analytic atan
    sinh = analytic sinh
    cosh = analytic cosh
    asinh = analytic asinh
    acosh = analytic acosh
    atanh = analytic atanh

-- |Printer
instance Show DoubleD where
    show (DoubleD 0 b) =
        "[~:" ++ show (b::Double) ++
        "; exactly zero]"
    show (DoubleD a b) =
        "[~:" ++ show (b::Double) ++
        "; d:" ++
        show (fromRational (imprecision (DoubleD a b))::Float) ++ "]"