{-
 - numbers/qrational.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Numbers.QRational
    (QRational)
where

import KryukovLib.Classes.Number

-- |QRational type is wrapper of Rational type with
-- support of functions (sin, cos, exp, log etc.)
-- and Number class
data QRational = QRational Rational deriving (Read, Eq, Ord)

-- |Wrapper for function of one parameter
f1 :: (Rational -> Rational) ->
    (QRational -> QRational)
f1 func = \(QRational a) -> QRational (func a)

-- |Wrapper for function of two parameters
f2 :: (Rational -> Rational -> Rational)
    -> (QRational -> QRational -> QRational)
f2 func = \(QRational b) -> \(QRational a) -> (QRational (func a b))

instance Num QRational where
    (+) = f2 (+)
    negate = f1 negate
    (*) = f2 (*)
    abs = f1 abs
    signum = f1 abs
    fromInteger a = QRational (fromInteger a)

instance Fractional QRational where
    fromRational c = QRational c
    recip = f1 recip

instance Real QRational where
    toRational (QRational a) = a

instance Show QRational where
    show (QRational a) = show a

instance Number QRational where
    toPrecise = QRational . toRational
    fromPrecise (QRational a) = fromRational a
    analytic func =
        \(QRational a) -> QRational ((toRational . func . fromRational) a)

instance Floating QRational where
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