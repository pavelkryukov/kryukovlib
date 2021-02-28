{-
 - core.hs
 -
 - Course work "Reverse interpolation problem"
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module Course9.Core
    (getSolution)
where

import KryukovLib.Classes.Number

import KryukovLib.Numbers.DoubleD

import KryukovLib.Algorithms.Interpolation
import KryukovLib.Algorithms.Interpolation.Lagrange
import KryukovLib.Algorithms.Interpolation.Newton

import KryukovLib.Solutions (inverseA)

import Course9.Functions

parseInterpolation :: [Char] -> Interpolation DoubleD DoubleD
parseInterpolation "-l" = lagrange
parseInterpolation "-n" = newton
parseInterpolation _    = help

help :: a
help = error
    ("\n\nInvalid input\n\n"
    ++ "Arguments, separated with space:\n"
    ++ "<interpolation type {-l;-n}>\n"
    ++ "<first node>\n"
    ++ "<last node>\n"
    ++ "<amount of nodes>\n"
    ++ "<function {cos, sin, exp, poly, sqr, cosx}>\n"
    ++ "<value on the right>")

getSolution :: [[Char]] -> DoubleD
getSolution args | length args /= 6 = help
                 | otherwise        =
    (inverseA
        (parseInterpolation (args !! 0))     -- interpolation
        (toPrecise (read (args !! 1)), toPrecise (read (args !! 2))) -- interval
        (read (args !! 3))                   -- nodes
        (parseFunction (args !! 4))          -- function
        (toPrecise (read (args !! 5)))             -- right
    )