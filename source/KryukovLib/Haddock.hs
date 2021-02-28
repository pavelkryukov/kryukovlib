module Main where

import KryukovLib.Algorithms.Advection.Corner
import KryukovLib.Algorithms.Advection.ImplicitCorner
import KryukovLib.Algorithms.Advection
import KryukovLib.Algorithms.Cauchy.Euler
import KryukovLib.Algorithms.Cauchy.Heun
import KryukovLib.Algorithms.Cauchy.RungeKutta4
import KryukovLib.Algorithms.Cauchy
import KryukovLib.Algorithms.Derivative.OneSided
import KryukovLib.Algorithms.Derivative.Symmetric
import KryukovLib.Algorithms.Derivative
import KryukovLib.Algorithms.Hopf.HopfCorner
import KryukovLib.Algorithms.Hopf.Lax
import KryukovLib.Algorithms.Hopf
import KryukovLib.Algorithms.Integ.Rectangles
import KryukovLib.Algorithms.Integ.Simpson
import KryukovLib.Algorithms.Integ.Trapezoid
import KryukovLib.Algorithms.Integ
import KryukovLib.Algorithms.Interpolation.Lagrange
import KryukovLib.Algorithms.Interpolation.Newton
import KryukovLib.Algorithms.Interpolation
import KryukovLib.Algorithms.MatrixInv.SLAEInverse
import KryukovLib.Algorithms.MatrixInv
import KryukovLib.Algorithms.SLAE.Gauss
import KryukovLib.Algorithms.SLAE.Jacobi
import KryukovLib.Algorithms.SLAE.Parameter
import KryukovLib.Algorithms.SLAE.Seidel
import KryukovLib.Algorithms.SLAE
import KryukovLib.Analyzer.Function.Derivative
import KryukovLib.Analyzer.Function.Simplifier
import KryukovLib.Analyzer.Function
import KryukovLib.Classes.CrossMult
import KryukovLib.Classes.LAO
import KryukovLib.Classes.Number
import KryukovLib.Classes.Semigroup
import KryukovLib.Common.Convergentor
import KryukovLib.Common.FinDiff
import KryukovLib.Common.Thomas
import KryukovLib.Common.VectorFunction
import KryukovLib.Coverage
import KryukovLib.Generic.Debug
import KryukovLib.Generic.ListFunctions
import KryukovLib.Generic.Peano
import KryukovLib.Generic.Polynoms
import KryukovLib.Numbers.DoubleD
import KryukovLib.Numbers.QRational
import KryukovLib.Solutions
import KryukovLib.Types.Complex
import KryukovLib.Types.Matrix
import KryukovLib.Types.Table
import KryukovLib.Types.Vector

main :: IO ()
main = putStrLn ""