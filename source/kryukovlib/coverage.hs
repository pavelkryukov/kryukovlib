{-
 - coverage.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}
module KryukovLib.Coverage
    (allTests)
where

import Prelude hiding (Num(..))

import Data.Maybe (fromJust)

import KryukovLib.Generic.ListFunctions (hgrid)
import KryukovLib.Generic.Peano

import KryukovLib.Classes.LAO
import KryukovLib.Classes.Semigroup

--import KryukovLib.Numbers.QRational
import KryukovLib.Numbers.DoubleD

import KryukovLib.Types.Complex
import KryukovLib.Types.Matrix
import KryukovLib.Types.Vector
import KryukovLib.Types.Table

import KryukovLib.Common.Thomas (thomas)
import KryukovLib.Common.Convergentor (convergentor)

import KryukovLib.Algorithms.SLAE
import KryukovLib.Algorithms.SLAE.Gauss
import KryukovLib.Algorithms.SLAE.Parameter
import KryukovLib.Algorithms.SLAE.Jacobi
import KryukovLib.Algorithms.SLAE.Seidel

import KryukovLib.Algorithms.Derivative
import KryukovLib.Algorithms.Derivative.Symmetric
import KryukovLib.Algorithms.Derivative.OneSided

import KryukovLib.Algorithms.Integ
import KryukovLib.Algorithms.Integ.Rectangles
import KryukovLib.Algorithms.Integ.Trapezoid
import KryukovLib.Algorithms.Integ.Simpson

import KryukovLib.Algorithms.Interpolation
import KryukovLib.Algorithms.Interpolation.Lagrange
import KryukovLib.Algorithms.Interpolation.Newton

--import KryukovLib.Algorithms.MatrixInv
import KryukovLib.Algorithms.MatrixInv.SLAEInverse

import KryukovLib.Algorithms.Cauchy
import KryukovLib.Algorithms.Cauchy.Euler
import KryukovLib.Algorithms.Cauchy.Heun
import KryukovLib.Algorithms.Cauchy.RungeKutta4

import KryukovLib.Analyzer.Function
import KryukovLib.Analyzer.Function.Derivative
import KryukovLib.Analyzer.Function.Simplifier

import KryukovLib.Solutions

type T = Complex DoubleD
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
derivate :: Derivate T (Vector Two T) -> IO ()
derivate alg =
     print $
        ((alg twofunction) 1)

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
        ((thomas
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
        Just invmatrix -> print ((invmatrix * testmatrix)::Matrix Two Two T)
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

-------------------------
-- Analytic derivative --
-------------------------

anfunction :: Function T
anfunction = simplifier (Mul (Mul Id Id) Id)

anderiv :: Int -> IO()
anderiv 0 = print $ 
    (converter anfunction) 1
anderiv 1 = print $
    (converter (derivative anfunction)) 1
anderiv 2 = print $
    (converter (derivative (derivative anfunction))) 1
anderiv 3 = print $ 
    (converter (derivative (derivative (derivative anfunction)))) 1
anderiv _ = print ""

---------------------
-- ALL TESTS LAUCH --
---------------------
allTests :: IO ()
allTests =
    splitter >>
    putStrLn "integrals" >>
    (integ rectInteg) >> (integ trapezoid) >> (integ simpson) >>
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
    (derivate (deriv 2 0.00001)) >> (derivate (derivS 2 0.00001)) >> 
        (derivate (derivS 2 0.0001))>>
    --
    splitter >>
    putStrLn "slae" >>
    (slae (parameter 0.5)) >> (slae gauss) >> (slae jacobi) >> (slae seidel) >>
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
    (ode euler) >> (ode heun) >> (ode rungekutta4) >>
    --
    splitter >>
    putStrLn "supermatrix" >>
    omg >>
    --
    splitter >>
    foldl (>>) (putStrLn "analytic derivative") (map anderiv [0..3])
