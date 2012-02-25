{-
 - functions.hs
 -
 - Course work "Reverse interpolation problem"
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module Course9.Functions
    (parseFunction)
where

parseFunction :: (Floating t) => [Char] -> (t -> t)
parseFunction "cos" = cos
parseFunction "sin" = sin
parseFunction "exp" = exp
parseFunction "poly" = \x -> x ** 3 + x ** 2
parseFunction "sqr"  = (** 2)
parseFunction "cosx" =
    \x -> (cos x) - x
parseFunction _      = error "Invalid function name"