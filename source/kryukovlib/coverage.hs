{-
 - coverage.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Coverage
    (allTests)
where

import Data.Maybe (fromJust)

import KryukovLib.Generic.ListFunctions (hgrid)

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Semigroup

import KryukovLib.Numbers.DoubleD

import KryukovLib.Types.Complex
import KryukovLib.Types.Matrix
import KryukovLib.Types.Vector
import KryukovLib.Types.Size
import KryukovLib.Types.Table

import KryukovLib.Common.DiagonalSLAE (solve3)
import KryukovLib.Common.Convergentor (convergentor)

import KryukovLib.Algorithms.SLAE
import KryukovLib.Algorithms.SLAE.Gauss
import KryukovLib.Algorithms.SLAE.Parameter
import KryukovLib.Algorithms.SLAE.Jacobi

--import KryukovLib.Algorithms.Derivative
import KryukovLib.Algorithms.Derivative.Symmetric
import KryukovLib.Algorithms.Derivative.OneSided

import KryukovLib.Algorithms.Integ
import KryukovLib.Algorithms.Integ.Rectangles
import KryukovLib.Algorithms.Integ.Trapecies
import KryukovLib.Algorithms.Integ.Simpson

import KryukovLib.Algorithms.Interpolation
import KryukovLib.Algorithms.Interpolation.Lagrange
import KryukovLib.Algorithms.Interpolation.Newton

--import KryukovLib.Algorithms.MatrixInv
import KryukovLib.Algorithms.MatrixInv.SLAEInverse

import KryukovLib.Algorithms.Cauchy
import KryukovLib.Algorithms.Cauchy.Euler
import KryukovLib.Algorithms.Cauchy.EulerCorr
import KryukovLib.Algorithms.Cauchy.RungeKutta4

import KryukovLib.Solutions

type T = DoubleD
type OMG = SqrMatrix Two (Complex (SqrMatrix Two (Quaternion T)))

---------
-- OMG --
---------

omg :: IO()
omg = print (iden::OMG)

------------
-- Common --
------------
twofunction :: T -> (Vector Two T)
twofunction = \x -> Vector [x, x*x]

--------------
-- Integral --
--------------
integ :: (Integ T (Vector Two T)) -> IO ()
integ alg =
    print $ integA alg (0,1) 601 twofunction

-------------------
-- Interpolation --
-------------------
interpolation :: Interpolation T T -> IO ()
interpolation alg =
    print $ (inverseA alg (0, 1) 15 (\x -> (cos x) - x)) 0
-------------------
-- Convergention --
-------------------
convergence :: IO ()
convergence =
    case (convergentor cos 1) of
        Just f -> print (f::T)
        Nothing -> putStrLn "No convergence"

----------------
-- Derivation --
----------------
derivateS :: IO ()
derivateS =
     print $
        sum [
                ((hderiv 2 (derivS 2 0.00001) sin)::T->T) (fromInteger n) +
                sin (fromInteger n)
            | n <- [0..100]]

derivate :: IO ()
derivate =
     print $
        ((hderiv 2 (deriv 1 0.00001) sin)::T->T) 0 + sin 0

derivate2 :: IO ()
derivate2 =
     print $
        (((deriv 2 0.00001) twofunction) 1)

----------
-- SLAE --
----------
slae :: SLAESolver Two T -> IO ()
slae alg =
    print $ alg testsystem
    where
        testsystem = SLAE (diag (Vector [3,2])) (Vector [1,1])

-----------------
-- 3 diag SLAE --
-----------------
slae3 :: IO ()
slae3 =
    print $
        ((solve3
            (Vector [2,2,2])
            (Vector [1,1,1])
            (Vector [2,2,2])
            (Vector [5,10,7]))::Vector Three T)

---------------
-- Invertion --
---------------
testmatrix :: Matrix Two Two T
testmatrix =
    Matrix
        [
        (Vector [3,2]),
        (Vector [4,3])
        ]

invertmatrix :: IO ()
invertmatrix =
    case ((slaeInverse gauss) testmatrix) of
        Just invmatrix -> print ((invmatrix <*> testmatrix)::Matrix Two Two T)
        Nothing -> putStrLn "Matrix couldn't be inverted"

condnumcounter :: (Norm (Matrix Two Two T)) -> IO ()
condnumcounter norm =
    print (fromJust ((condNumber (slaeInverse gauss) norm) testmatrix))

--------------------
-- Cauchy problem --
--------------------
ode :: CauchySolver T (Vector Three T) -> IO()
ode alg =
    print $ head (reverse values)
    where
        grid = hgrid (0, 1) 101
        function _ u = u
        startvector = Vector [2, -1, 0]
        (_,values) = unTable (alg function grid startvector)

splitter :: IO()
splitter = putStrLn "\n---------------------------"

---------------------
-- ALL TESTS LAUCH --
---------------------
allTests :: IO ()
allTests =
    splitter >>
    putStrLn "integrals" >>
    (integ rectInteg) >> (integ trapInteg) >> (integ simpson) >>
    --
    splitter >>
    putStrLn "interpolations" >>
    (interpolation newton) >> (interpolation lagrange) >>
    --
    splitter >>
    putStrLn "convergence" >>
    convergence >>
    --
    splitter >>
    putStrLn "derivates" >>
    derivateS >> derivate2 >> derivate >>
    --
    splitter >>
    putStrLn "slae" >>
    (slae (parameter 0.5)) >> (slae gauss) >> (slae jacobi) >>
    --
    splitter >>
    putStrLn "slae3" >>
    slae3 >>
    --
    splitter >>
    putStrLn "matrix invertion" >>
    invertmatrix >> (condnumcounter norm1) >> (condnumcounter norm2) >>
    --
    splitter >>
    putStrLn "cauchy" >>
    (ode euler) >> (ode eulercorr) >> (ode rungekutta4) >>
    --
    splitter >>
    putStrLn "supermatrix" >>
    omg