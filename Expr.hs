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

