module Expr where
 
import HParser

instance Show HVal where show = showVal
			 
showVal :: HVal -> String
showVal (HString string) = "\"" ++ show string ++ "\""
showVal (HInteger number) = show number
showVal (HBool True) = "True"
showVal (HBool False) = "False"
showVal (HList list) = "[" ++ unravel list ++ "]"

unravel :: [HVal] -> String
unravel list = unwords (map showVal list)


eval :: HVal -> HVal
---------- EVALUATING PRIMITIVES ----------
eval val@(HString _) = val
eval val@(HInteger _) = val
eval val@(HBool _) = val
eval val@(HList _) = val
eval (Expr x op y) = evalExpr x op y

evalExpr :: HVal -> Op -> HVal -> HVal

----------- Expression Evaulation of Atomic Values ----------
evalExpr (HInteger x) Add (HInteger y)  = HInteger (x + y)
evalExpr (HInteger x) Sub (HInteger y)  = HInteger (x - y)
evalExpr (HInteger x) Mult (HInteger y) = HInteger (x * y)
evalExpr (HInteger x) Div (HInteger y)  = HInteger (x `div` y)
evalExpr (HInteger x) Mod (HInteger y)  = HInteger (x `mod` y)
evalExpr (HBool x)    And (HBool y)     = HBool    (x && y)
evalExpr (HBool x)    Or  (HBool y)     = HBool    (x || y)
evalExpr (HInteger x) Greater (HInteger y)   = HBool (x > y)
evalExpr (HInteger x) GreaterEq (HInteger y) = HBool (x >= y)
evalExpr (HInteger x) Less    (HInteger y)   = HBool (x < y)
evalExpr (HInteger x) LessEq  (HInteger y)   = HBool (x <= y)
evalExpr (HInteger x) Equal   (HInteger y)   = HBool (x == y)
evalExpr (HBool x)    Equal   (HBool y)      = HBool (x == y)

----------- Expression Evaulation in Recursive Cases ----------
evalExpr (HInteger x) op (Expr a op' b) = evalExpr (HInteger x) op (evalExpr a op' b)
