{-
 - analyzer/function/derivative.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}

module KryukovLib.Analyzer.Function.Derivative
    (derivative)
where

import KryukovLib.Analyzer.Function

derivative :: Function -> Function
derivative (Sum a b) = Sum (derivative a) (derivative b)
derivative (Mul a b) = Sum (Mul (derivative a) b) (Mul (derivative b) a)
derivative (Comp a b) = Mul (derivative b) (Comp (derivative a) b)
derivative Sh = Ch
derivative Sin = Cos
derivative Ch = Sh
derivative Cos = Mul (Const (-1)) Sin
derivative Exp = Exp
derivative Id = Const 1
derivative Ln = Power (-1)
derivative (Const _) = Const 0
derivative (Power p) = Mul (Const p) (Power (p-1))

