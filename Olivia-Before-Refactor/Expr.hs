{-# LANGUAGE LambdaCase #-}
module Expr where
 
import HParser

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
import Data.Char

instance Show HVal where show = showVal
type Env = IORef [(String, IORef HVal)] 

showVal :: HVal -> String
showVal (HString string) = "\"" ++ show string ++ "\""
showVal (HInteger number) = show number
showVal (HBool True) = "True"
showVal (HBool False) = "False"
showVal (HList list) = "[" ++ unravel list ++ "]"
showVal (Expr x op y) = show x ++ " " ++ show op ++ " " ++ show y
showVal (VarExpr x op y) = show x ++ " " ++ show op ++ " " ++ show y
showVal (If cond expr expr') = "if (" ++ show cond ++ ")->" ++ show expr++ show expr'
showVal (SubIf cond expr) = "[] (" ++ show cond ++ ")->" ++ show expr
showVal (Do cond expr) = "Do (" ++ show cond ++ ")->" ++ "\n" ++ show expr
showVal (Assign var val) = show var ++ " := " ++ show val ++ "\n"
showVal (Program program) = show program


unravel :: [HVal] -> String
unravel list = unwords (map showVal list)


eval :: Env -> HVal -> IOThrowsError HVal

---------- EVALUATING PRIMITIVES ----------
eval env val@(HString _)  = return val
eval env val@(HInteger _) = return val
eval env val@(HBool _)    = return val
eval env val@(HList _)    = return val

---------- EVLAUATING EXPRESSIONS ----------
eval env (Expr x op y)    = evalExpr env x op y
eval env (Do cond expr)   = doo env cond expr
eval env (Assign var val) = eval env val >>= defineVar env var

while :: (Monad m) => m Bool -> m a -> m ()
while cond action = do
    c <- cond
    when c $ do
        action
        while cond action

f :: Env -> HVal -> [HVal] -> IOThrowsError HVal
f env cond expr = do
        traverse_ (eval env) expr
        eval env $ Do cond expr

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ condition body = loop
  where
    loop = condition >>= \ case
      True -> body >> loop
      False -> pure ()

doo :: Env -> HVal -> [HVal] -> IOThrowsError ()
doo env cond expr = do
  traverse_ (eval env) expr
  whileM_ (isTrue <$> eval env cond) (traverse_ (eval env) expr)

isTrue :: HVal -> Bool
isTrue (HBool True) = True
isTrue (HBool False) = False

evalDo :: Env -> HVal -> [HVal] -> IOThrowsError HVal
evalDo env cond expr = eval env cond >>= \x -> case x of
                                                HBool False -> return $ HString "Success"
                                                HBool True  -> do
                                                            traverse_ (eval env) expr
                                                            eval env $ Do cond expr
                                                                  --return $ map (\x -> eval env x) expr >>= \y -> return $ eval env $ Do cond expr
--evalDo env cond expr = eval env cond >>= \x -> case x of
  --                                               HBool True  -> return $ map (eval env) expr >>= \y -> return $ eval env (Do cond expr)
    --                                             HBool False -> 1


--evalDo :: Env -> HVal -> HVal -> IOThrowsError HVal
--evalDo env cond expr = eval env cond >>= \x -> case x of
  --                                                  HBool True  -> eval env expr >>= \y -> evalDo env x y --eval env $ Do cond (expr)
    --                                                HBool False -> return $ HInteger 1
                                                    --eval env expr >>= \y -> eval env $ Do cond y

-- May add function : evalExpr env (Expr _ _ _) op (HVal _)   .... Not sure if it's a good idea or not.
evalExpr :: Env -> HVal -> Op -> HVal -> IOThrowsError HVal

----------- Expression Evaulation of Integers ----------
evalExpr env (HInteger x) Add       (HInteger y)   = return $ HInteger (x + y)
evalExpr env (HInteger x) Sub       (HInteger y)   = return $ HInteger (x - y)
evalExpr env (HInteger x) Mult      (HInteger y)   = return $ HInteger (x * y)
evalExpr env (HInteger x) Div       (HInteger y)   = return $ HInteger (x `div` y)
evalExpr env (HInteger x) Mod       (HInteger y)   = return $ HInteger (x `mod` y)
evalExpr env (HInteger x) Greater   (HInteger y)   = return $ HBool (x > y)
evalExpr env (HInteger x) GreaterEq (HInteger y)   = return $ HBool (x >= y)
evalExpr env (HInteger x) Less      (HInteger y)   = return $ HBool (x < y)
evalExpr env (HInteger x) LessEq    (HInteger y)   = return $ HBool (x <= y)
evalExpr env (HInteger x) Equal     (HInteger y)   = return $ HBool (x == y)

----------- Expression Evaluation of Booleans ----------
evalExpr env (HBool x)    And      (HBool y)       = return $ HBool (x && y)
evalExpr env (HBool x)    Or       (HBool y)       = return $ HBool (x || y)
evalExpr env (HBool x)    Equal    (HBool y)       = return $ HBool (x == y)

----------- Expression Evaluation of Variables ---------- 
evalExpr env (HString x)  op       (HInteger y)    = getVar env x >>= (\a -> evalExpr env a op (HInteger y))
evalExpr env (HInteger x) op       (HString y)     = getVar env y >>= (\a -> evalExpr env (HInteger x) op a)
evalExpr env (HString x)  op       (HString y)     = getVar env x >>= (\a -> getVar env y >>= (\b -> evalExpr env a op b))
evalExpr env (HString x)  op       (HBool y)       = getVar env x >>= (\a -> evalExpr env a op (HBool y))
evalExpr env (HBool x)    op       (HString y)     = getVar env y >>= (\a -> evalExpr env (HBool x) op a)

----------- Expression Evalation in Recursive Cases ----------
evalExpr env (HInteger x) op      (Expr a op' b)   = eval env (Expr a op' b) >>= \y -> evalExpr env (HInteger x) op y
evalExpr env (HString  x) op      (Expr a op' b)   = eval env (Expr a op' b) >>= \y -> getVar env x >>= \y' -> evalExpr env y op y'
evalExpr env (HBool    x) op      (Expr a op' b)   = eval env (Expr a op' b) >>= \y -> evalExpr env (HBool    x) op y


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


