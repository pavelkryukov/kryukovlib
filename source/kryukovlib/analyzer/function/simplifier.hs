{-
 - analyzer/function/simplifier.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}

module KryukovLib.Analyzer.Function.Simplifier
    (simplifier)
where

import KryukovLib.Analyzer.Function

simplifier :: Function -> Function
simplifier (Sum (Const a) (Const b)) = Const (a + b)
simplifier (Sum (Const 0) b) = simplifier b
simplifier (Mul (Const a) (Const b)) = Const (a * b)
simplifier (Mul (Const 1) b) = simplifier b
simplifier (Comp Id b) = simplifier b
simplifier (Comp b Id) = simplifier b
simplifier (Sum a b)
    | a == b    = Mul (Const 2) (simplifier a)
    | otherwise = Sum (simplifier a) (simplifier b)
simplifier (Mul a b)
    | a == b    = Comp (Power 2) (simplifier a)
    | otherwise = Mul (simplifier a) (simplifier b)
simplifier (Comp Exp Ln) = Id
simplifier t = t