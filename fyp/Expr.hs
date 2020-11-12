module Expr where

import Parser


import System.Exit
import Data.Foldable
import System.Environment
import System.IO
import Prelude hiding (tail)
import Data.IORef
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Char hiding (spaces)
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

--type Env = IORef [(String, IORef HVal)]

instance Show HVal where show = showVal

showVal :: HVal -> String
showVal (HInteger val)   = show val
showVal (HBool   True)   = "True"
showVal (HBool  False)   = "False"
showVal (HString  val)   = val
showVal (HList    val)   = "[" ++ show val ++ "]"
showVal (Arith x op y)   = show x ++ " " ++ show op ++ " " ++ show y

evalArithmetic :: HVal -> Op -> HVal -> HVal
evalArithmetic (HInteger x) Add  (HInteger y) = HInteger (x + y)
evalArithmetic (HInteger x) Sub  (HInteger y) = HInteger (x - y)
evalArithmetic (HInteger x) Mult (HInteger y) = HInteger (x * y)
evalArithmetic (HInteger x) Div  (HInteger y) = HInteger (x `div` y)
evalArithmetic (HInteger x) Mod  (HInteger y) = HInteger (x `mod` y)
evalArithmetic (HInteger x) op   (Arith x' op' y') = evalArithmetic (HInteger x) op (evalArithmetic x' op' y')
evalArithmetic (HBool    x) And  (HBool y)    = HBool (x && y)
evalArithmetic (HBool    x) Or   (HBool y)    = HBool (x || y)
evalArithmetic (HBool    x) op   (Arith x' op' y') = evalArithmetic (HBool x) op (evalArithmetic x' op' y')

evalVal :: HVal -> HVal
evalVal val @(HInteger _) = val
evalVal val @(HBool    _) = val
evalVal val @(HString  _) = val
evalVal val @(HList    _) = val
evalVal (Arith x op y) = evalArithmetic x op y

