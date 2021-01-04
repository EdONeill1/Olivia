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


instance Show HVal where show = showVal
instance Show HStatement where show = showStatement

showVal :: HVal -> String
showVal (HInteger val)   = show val
showVal (HBool   True)   = "True"
showVal (HBool  False)   = "False"
showVal (HString  val)   = val
showVal (HList    val)   = "[" ++ show val ++ "]"
showVal (Arith x op y)   = show x ++ " " ++ show op ++ " " ++ show y

showStatement :: HStatement -> String
showStatement (Eval val) = showVal val

evalArithmetic :: HVal -> Op -> HVal -> HVal
evalArithmetic (HInteger x) Add  (HInteger y) = HInteger (x + y)
evalArithmetic (HInteger x) Sub  (HInteger y) = HInteger (x - y)
evalArithmetic (HInteger x) Mult (HInteger y) = HInteger (x * y)
evalArithmetic (HInteger x) Div  (HInteger y) = HInteger (x `div` y)
evalArithmetic (HInteger x) Mod  (HInteger y) = HInteger (x `mod` y)
evalArithmetic (HInteger x) Max  (HInteger y) = if x > y then (HInteger x) else (HInteger y)
evalArithmetic (HInteger x) Min  (HInteger y) = if x < y then (HInteger x) else (HInteger y)
evalArithmetic (HInteger x) op   (Arith x' op' y') = evalArithmetic (HInteger x) op (evalArithmetic x' op' y')
evalArithmetic (HBool    x) And  (HBool y)    = HBool (x && y)
evalArithmetic (HBool    x) Or   (HBool y)    = HBool (x || y)
evalArithmetic (HBool    x) op   (Arith x' op' y') = evalArithmetic (HBool x) op (evalArithmetic x' op' y')

evalIOVal :: IO HVal -> IO HVal
evalIOVal val = val

evalVal :: HVal -> HVal
evalVal val @(HInteger _) = val
evalVal val @(HBool    _) = val
evalVal val @(HString  _) = val
evalVal val @(HList    _) = val
evalVal (Arith x op y) = evalArithmetic x op y

evalStatement :: HStatement -> HVal
evalStatement (Eval val) = evalVal val

