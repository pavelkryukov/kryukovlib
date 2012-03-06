data Elementary = Sh | Ch | Exp | Const Double | Zero deriving (Eq)
data Function = 
    Function Elementary |
    Comp Function Function |
    Mult Function Function |
    Sum Function Function deriving (Eq)

derivative :: Function -> Function
derivative (Sum a b) = Sum (derivative a) (derivative a)
derivative (Mult a b) = Sum (Mult (derivative a) b) (Mult (derivative b) a)
derivative (Comp a b) = Mult (derivative b) (Comp (derivative a) b)
derivative (Function Sh) = Function Ch
derivative (Function Ch) = Function Sh
derivative (Function Exp) = Function Exp 
derivative (Function (Const _)) = Function Zero
derivative (Function Zero) = Function Zero

converter :: Function -> (Double -> Double)
converter (Sum a b) = \x -> (converter a x) + (converter b x)
converter (Mult a b) = \x -> (converter a x) * (converter b x)
converter (Comp a b) = (converter a) . (converter b)
converter (Function Sh) = sinh
converter (Function Ch) = cosh
converter (Function Exp) = exp
converter (Function (Const cnst)) = \_ -> cnst
converter (Function Zero) = \_ -> 0

printer :: Function -> [Char]
printer (Function Sh) = "sh"
printer (Function Ch) = "ch"
printer (Function Exp) = "exp"
printer (Function (Const a)) = show a
printer (Function Zero) = "0"
printer (Sum a b) = printer a ++ " + " ++ printer b
printer (Mult a b) = "(" ++ printer a ++ "*" ++ printer b ++ ")"
printer (Comp a b) = printer a ++ "(" ++ printer b ++ ")"

simplifier :: Function -> Function
simplifier (Sum a b)
    | a == b    = Mult (Function (Const 2)) (simplifier a)
    | otherwise = Sum (simplifier a) (simplifier b)
simplifier t = t

main :: IO()
main = print $ printer (simplifier (derivative (Mult (Function Sh) (Function Ch))))