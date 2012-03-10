{-
 - analyzer/function.hs
 -
 - Kryukov computational mathematics library (KryukovLib)
 - Copyright (C) Pavel Kryukov, 2011-2012
-}

module KryukovLib.Analyzer.Function
    (Function(..), converter, show')
where
   
data Function =
    Id                     |
    Const Double           |
    Sin                    |
    Cos                    |
    Sh                     |
    Ch                     |
    Exp                    |
    Ln                     |
    Power Double           |
    Comp Function Function |
    Mul Function Function  |
    Sum Function Function   deriving (Eq)
    
show' :: Function -> [Char]
show' Sh = "sh"
show' Ch = "ch"
show' Sin = "sin"
show' Cos = "cos"
show' Exp = "exp"
show' Ln = "ln"
show' Id = "id"
show' (Const a) = show a
show' (Sum a b) = show' a ++ " + " ++ show' b
show' (Mul a b) = "(" ++ show' a ++ "*" ++ show' b ++ ")"
show' (Comp a b) = show' a ++ "(" ++ show' b ++ ")"
show' (Power a) = "pwr" ++ show a

converter :: Function -> (Double -> Double)
converter (Sum a b) = \x -> (converter a x) + (converter b x)
converter (Mul a b) = \x -> (converter a x) * (converter b x)
converter (Comp a b) = (converter a) . (converter b)
converter Sh = sinh
converter Sin = sin
converter Ch = cosh
converter Cos = cos
converter Exp = exp
converter Ln = log
converter Id = id
converter (Const cnst) = \_ -> cnst
converter (Power pwr) = (** pwr)
