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

type Env = IORef [(String, IORef HVal)]

instance Show HVal where show = showVal
instance Show Statement where show = showStatement

showVal :: HVal -> String
showVal (HInteger val) = show val
showVal (HBool   True) = "True"
showVal (HBool  False) = "False"
showVal (HString  val) = val
showVal (HList    val) = "[" ++ show val ++ "]"
showVal (Expr x op y ) = show x ++ " " ++ show op ++ " " ++ show y

showStatement :: Statement -> String
showStatement (Assign var val) = show var ++ " := " ++ show val
showStatement (Do cond expr)   = "Do (" ++ show cond ++ ")->" ++ show expr

evalHVal :: Env -> HVal -> IOThrowsError HVal
evalHVal env val @(HInteger _) = return $ val
evalHVal env val @(HBool    _) = return $ val
evalHVal env val @(HString  _) = return $ val
evalHVal env val @(HList    _) = return $ val
evalHVal env (Expr x op y)     = evalExpr env x op y

evalExpr :: Env -> HVal -> Op -> HVal -> IOThrowsError HVal
evalExpr env (HInteger x) Add  (HInteger y) = return $ HInteger (x   +   y)
evalExpr env (HInteger x) Sub  (HInteger y) = return $ HInteger (x   -   y)
evalExpr env (HInteger x) Mult (HInteger y) = return $ HInteger (x   *   y)
evalExpr env (HInteger x) Div  (HInteger y) = return $ HInteger (x `div` y)
evalExpr env (HInteger x) Mod  (HInteger y) = return $ HInteger (x `mod` y)

evalExpr env (HInteger x) op (Expr x' op' y') = evalExpr env x' op' y' >>= \y -> evalExpr env (HInteger x) op y 

evalExpr env (HBool x) And (HBool y) = return $ HBool (x && y)
evalExpr env (HBool x) Or  (HBool y) = return $ HBool (x || y)




unravel :: [HVal] -> String
unravel list = unwords (map showVal list)

--------------------------------------------------------------
             ------ Error Handling -----
--------------------------------------------------------------
       
data HError = NumArgs Integer [HVal]
               | TypeMismatch String HVal
               | Parser ParseError
               | BadSpecialForm String HVal 
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: HError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unravel found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show HError where show = showError

type ThrowsError = Either HError
type IOThrowsError = ExceptT HError IO

trapError action = catchError action (return . show)


extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-------------------------------
----- Variable Assignment -----
-------------------------------

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError HVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> HVal -> IOThrowsError HVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> HVal -> IOThrowsError HVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: Env -> [(String, HVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)














