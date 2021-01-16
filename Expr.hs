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
import Text.Parsec.Char

instance Show HVal where show = showVal
instance Show HStatement where show = showStatement

showVal :: HVal -> String
showVal (HInteger val)   = show val ++ " "
showVal (HBool   True)   = "True "
showVal (HBool  False)   = "False "
showVal (HString  val)   = val ++ "\n"
showVal (HList    val)   = "[" ++ show val ++ "] "
showVal (Arith x op y)   = show x ++ " " ++ show op ++ " " ++ show y ++ " "
showVal (Assign var val) = show var ++ " := " ++ show val ++ " "

showStatement :: HStatement -> String
showStatement (Eval val)     = showVal val
showStatement (Print val)    = "\nPrint (" ++ showVal val ++ ")\n"
showStatement (Do cond expr) = "\nDo (" ++ show cond ++ ")->\n" ++ show expr ++"\nOd"
showStatement (If cond expr) = unlines $ map (showStatement) expr

evalArithmetic :: Env -> HVal -> Op -> HVal -> IOThrowsError HVal
evalArithmetic env (HInteger x) Add  (HInteger y) = return $ HInteger (x + y)
evalArithmetic env (HInteger x) Sub  (HInteger y) = return $ HInteger (x - y)
evalArithmetic env (HInteger x) Mult (HInteger y) = return $ HInteger (x * y)
evalArithmetic env (HInteger x) Div  (HInteger y) = return $ HInteger (x `div` y)
evalArithmetic env (HInteger x) Mod  (HInteger y) = return $ HInteger (x `mod` y)
evalArithmetic env (HInteger x) Max  (HInteger y) = if x > y then return $ (HInteger x) else return $ (HInteger y)
evalArithmetic env (HInteger x) Min  (HInteger y) = if x < y then return $ (HInteger x) else return $ (HInteger y)
evalArithmetic env (HInteger x) Less (HInteger y) = if x < y then return $ (HBool True) else return $ (HBool False)
evalArithmetic env (HInteger x) Greater (HInteger y) = if x > y then return $ (HBool True) else return $ (HBool False)
evalArithmetic env (HInteger x) op   (Arith x' op' y') = evalArithmetic env x' op' y' >>= \y -> evalArithmetic env (HInteger x) op y
evalArithmetic env (HBool    x) And  (HBool y)    = return $ HBool (x && y)
evalArithmetic env (HBool    x) Or   (HBool y)    = return $ HBool (x || y)
evalArithmetic env (HBool    x) op   (Arith x' op' y') = evalArithmetic env x' op' y' >>= \y -> evalArithmetic env (HBool x) op y
----- Variable Arithemtic -----
evalArithmetic env (HString  x)  op   (HInteger y) = getVar env x >>= (\a -> evalArithmetic env a op (HInteger y))
evalArithmetic env (HInteger x)  op   (HString  y) = getVar env y >>= (\a -> evalArithmetic env (HInteger x) op a)
evalArithmetic env (HString  x)  op   (HString  y) = getVar env x >>= (\a -> getVar env y >>= (\b -> evalArithmetic env a op b)) 

evalIOVal :: IO HVal -> IO HVal
evalIOVal val = val

evalVal :: Env -> HVal -> IOThrowsError HVal
evalVal env val @(HInteger _) = return $ val
evalVal env val @(HBool    _) = return $ val
evalVal env val @(HString  _) = return $ val
evalVal env val @(HList    _) = return $ val
evalVal env (Arith x op y)    = evalArithmetic env x op y
evalVal env (Assign var val)  = evalVal env val >>= defineVar env var



evalPrint :: Env -> HStatement -> IOThrowsError HVal
evalPrint env (Print (HString val)) = getVar env val >>= \x -> evalVal env x
                                

evalDo :: Env -> HStatement -> IOThrowsError ()
evalDo env (Do cond expr) = evalVal env cond >>= \x -> case x of 
                                                          HBool False -> return ()
                                                          HBool True  -> do
                                                                  traverse_ (evalStatement_ env) expr
                                                                  evalStatement_ env $ Do (HBool False) expr


evalStatement_ :: Env -> HStatement -> IOThrowsError ()
evalStatement_ env (Do cond expr) = evalVal env cond >>= \x -> case x of
                                                                 HBool False -> return ()
                                                                 HBool True  -> do
                                                                         traverse_ (evalStatement_ env) expr
                                                                         evalStatement_ env (Do cond expr)

evalStatement_ env (Skip skip) = return () 
evalStatement_ env (Print (HString val)) = getVar env val >>= \x -> liftIO $ putStrLn $ show x
evalStatement_ env (Print val) = liftIO $ putStrLn $ show val
evalStatement_ env (Eval val) = do
    result <- evalVal env val
    return ()

evalStatement_ env (If cond expr) = evalVal env cond >>= \x -> case x of
                                                                     HBool False -> return ()
                                                                     HBool True  -> traverse_ (evalStatement_ env) expr
                                                                    
                                                                              

evalStatement :: Env -> HStatement -> IOThrowsError HVal
evalStatement env (Eval val)  = evalVal env val
--evalStatement env (Print val) = return $ val
--evalStatement env (Do cond expr) = evalDo env <$> (Do cond expr)

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
               | Statement String

showError :: HError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unravel found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Statement statement) = "Error in statement: " ++ show statement

instance Show HError where show = showError

type ThrowsError = Either HError
type IOThrowsError = ExceptT HError IO

trapError action = catchError action (return . show)


extractValue :: ThrowsError a -> a
extractValue (Right val) = val
-------------------------------
----- Variable Assignment -----
-------------------------------
type Env = IORef [(String, IORef HVal)]

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


