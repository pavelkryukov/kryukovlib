{-
 - analytic.hs
 -
 - Course work "Reverse interpolation problem"
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module Course9.Analytic
    (analyse)
where

import KryukovLib.Numbers.DoubleD

import KryukovLib.Algorithms.Interpolation.Lagrange

import KryukovLib.Solutions (inverseA)

import Course9.Functions

analyse :: Int -> IO()
analyse 1 =
    (putStrLn "lagrange, x^2 = 1, [1/2^n; 2], 256 nodes:") >>
    (print $
    [
        (inverseA
            (lagrange)
            ((0.5)^(n::Integer), 2)
            256
            (parseFunction "sqr")
            1
        )::DoubleD
        | n <- [1..8]
    ])

analyse 2 =
    (putStrLn "lagrange, x^2 = 2, [1/2^n; 2], 256 nodes:") >>
    (print $
    [
        (inverseA
            (lagrange)
            ((0.5)^(n::Integer), 2)
            256
            (parseFunction "sqr")
            2
        )::DoubleD
        | n <- [1..8]
    ])

analyse 3 =
    (putStrLn "lagrange, x^2 = 2, [1/2; 2], (2^(n+4) / 10) nodes:") >>
    (print $
    [
        (inverseA
            (lagrange)
            (0.5, 2)
            (round (2^(n::Integer) / (10::Double)))
            (parseFunction "sqr")
            2
        )::DoubleD
        | n <- [4..12]
    ])

analyse 4 =
    (putStrLn "lagrange, cos x = x, [0; 1], 15 nodes:") >>
    (print $
    (
        (inverseA
            (lagrange)
            (0, 1)
            15
            (parseFunction "cosx")
            0
        )::DoubleD
    ))

analyse 5 =
    (putStrLn "lagrange, x^3 + x^2 = 1, [1/2; 1], 15 nodes:") >>
    (print $
    (
        (inverseA
            (lagrange)
            (0.5, 1)
            15
            (parseFunction "poly")
            1
        )::DoubleD
    ))

analyse _ = error "No such analyse"
    