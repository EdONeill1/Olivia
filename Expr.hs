module Expr where

import Parser

import Data.Monoid (Alt(..))
import Data.Coerce (coerce)
import System.Exit
import Data.Foldable
import Data.List
import System.IO.Unsafe (unsafePerformIO)
import System.Random
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

showVal :: HVal -> [Char]
showVal (HInteger val)      = show val ++ " "
showVal (HBool   True)      = "True "
showVal (HBool  False)      = "False "
showVal (HString  val)      = val ++ " "
showVal (HValString val)    = show val
showVal (HList    val)      = show val
showVal (Arith x op y)      = show x ++ " " ++ show op ++ " " ++ show y ++ " "
showVal (Assign var val)    = show var ++ " := " ++ show val ++ " "
showVal (Length   val)      = "length." ++ show val
showVal (Cons list1 list2)  = show list1 ++ show list2
showVal (Car (HList (x:xs)))= show x
showVal (Cdr (HList (x:xs)))= show xs

showStatement :: HStatement -> [Char]
showStatement (Eval val)     = showVal val
showStatement (Print val)    = "\nPrint (" ++ showVal val ++ ")\n"
showStatement (Do cond expr) = "\nDo (" ++ show cond ++ ")->\n" ++ show expr ++"\nOd"
showStatement (If (cond, expr)) = unlines $ map (showStatement) expr
showStatement (Selection if_ selection fi_ n) = unlines $ map (showStatement) selection
showStatement (Ifs if_ ifs fi_ n) = unlines $ map (showStatement) ifs

evalArithmetic :: Env -> HVal -> Op -> HVal -> IOThrowsError HVal
evalArithmetic env (HInteger x) Add  (HInteger y)        = return $ HInteger (x + y)
evalArithmetic env (HInteger x) Sub  (HInteger y)        = return $ HInteger (x - y)
evalArithmetic env (HInteger x) Mult (HInteger y)        = return $ HInteger (x * y)
evalArithmetic env (HInteger x) Div  (HInteger y)        = return $ HInteger (x `div` y)
evalArithmetic env (HInteger x) Mod  (HInteger y)        = return $ HInteger (x `mod` y)
evalArithmetic env (HInteger x) Less (HInteger y)        = if x < y then return $ (HBool True) else return $ (HBool False)
evalArithmetic env (HInteger x) Greater (HInteger y)     = if x > y then return $ (HBool True) else return $ (HBool False)
evalArithmetic env (HInteger x) Leq  (HInteger y)        = if x <= y then return $ (HBool True) else return $ (HBool False)
evalArithmetic env (HInteger x) Geq  (HInteger y)        = if x >= y then return $ (HBool True) else return $ (HBool False)
evalArithmetic env (HInteger x) Up (HInteger y)          = if x > y then return $ (HInteger x) else return $ (HInteger y)
evalArithmetic env (HInteger x) Down (HInteger y)        = if x < y then return $ (HInteger x) else return $ (HInteger y)
evalArithmetic env (HInteger x)  op  (Arith x' op' y')   = evalArithmetic env x' op' y' >>= \y -> evalArithmetic env (HInteger x) op y
evalArithmetic env (HInteger x)  op  (HBool True)        = return $ HBool True
evalArithmetic env (HBool True)  op  (HInteger x)        = return $ HBool True
evalArithmetic env (HInteger x)  op  (HBool False)       = return $ HBool False
evalArithmetic env (HBool False) op  (HInteger x)        = return $ HBool False
evalArithmetic env (HInteger x)  op  (HList y)           = return $ HInteger 5
evalArithmetic env (HList x)     Dot  (HInteger y)       = return $ x !! fromIntegral(y)

----- Boolean Arithmetic -----
evalArithmetic env (HBool True)  And (HBool True)         = return $ HBool True
evalArithmetic env (HBool True)  And (HBool False)        = return $ HBool False

----- Integer op Assign _ _ -----
evalArithmetic env (HInteger x) op (Assign var val)      = evalVal env (Assign var val) >>= \v -> evalArithmetic env (HInteger x) op v 
-----                       -----

evalArithmetic env (Arith x' op' y') op (HInteger y)      = evalArithmetic env x' op' y' >>= \x -> evalArithmetic env x op (HInteger y)
evalArithmetic env (Arith x  op' y)  op (Arith a op'' b)  = evalArithmetic env x  op' y  >>= \var -> evalArithmetic env var op (Arith a op'' b)
evalArithmetic env (HString  x) op   (Arith x' op' y')    = evalArithmetic env x' op' y' >>= \y -> getVar env x >>= \var -> evalArithmetic env var op y
evalArithmetic env (Arith x' op' y') op (HString y)       = evalArithmetic env x' op' y' >>= \x -> getVar env y >>= \var -> evalArithmetic env x op var
evalArithmetic env (HBool    x) And  (HBool y)            = return $ HBool (x && y)
evalArithmetic env (HBool    x) Or   (HBool y)            = return $ HBool (x || y)
evalArithmetic env (HBool    x) op   (Arith x' op' y')    = evalArithmetic env x' op' y' >>= \y -> evalArithmetic env (HBool x) op y
evalArithmetic env (HInteger x) Equals  (HInteger y)      = if (HInteger x) == (HInteger y) then return $ (HBool True) else return $ (HBool False)
evalArithmetic env (HString  x) Equals  (HInteger y)      = getVar env x >>= \var -> if var == (HInteger y) then return $ (HBool True) else return $ (HBool False)
evalArithmetic env (HInteger x) Equals  (HString  y)      = getVar env y >>= \var -> if (HInteger x) == var then return $ (HBool True) else return $ (HBool False)
evalArithmetic env (HInteger x) NEquals (HInteger y)      = return $ HBool (x /= y)
evalArithmetic env (HString  x) NEquals (HInteger y)      = getVar env x >>= \var -> return $ HBool (var /= (HInteger y))
evalArithmetic env (HInteger x) NEquals (HString  y)      = getVar env y >>= \var -> return $ HBool ((HInteger x) /= var)

evalArithmetic env ((ListAccess (HList xs) (HInteger x))) op (HInteger y) = evalVal env ((ListAccess (HList xs) (HInteger x))) >>= \x' -> evalArithmetic env x' op (HInteger y) 
----- Variable Arithemtic -----
evalArithmetic env (HString  x)  op   (HInteger y) = getVar env x >>= (\a -> evalArithmetic env a op (HInteger y)) 
evalArithmetic env (HInteger x)  op   (HString  y) = getVar env y >>= (\a -> evalArithmetic env (HInteger x) op a)
evalArithmetic env (HString  x)  op   (HString  y) = getVar env x >>= (\a -> getVar env y >>= (\b -> evalArithmetic env a op b)) 

----- List Functionality ----- NOT YET CERTAIIN IF COMMENTED CODE IS NEEDED WITHIN CODEBASE
--evalArithmetic env (HList    x) Dot   (HInteger y)           = return $ x !! fromIntegral(y) 
--evalArithmetic env (HList    x) Dot   (HString  y)           = getVar env y >>= (\index -> evalVal env $ Arith (HList x) Dot index)
--evalArithmetic env (HInteger x) Dot   (HInteger y)           = return $ HInteger 0
--evalArithmetic env (HString  x) Dot   (HInteger y)           = getVar env x >>= \x' -> evalArithmetic env x' Dot (HInteger y)
evalArithmetic env (HString  x) op    (Length   (HList y))   = getVar env x >>= (\a -> evalArithmetic env a op (HInteger $ sum [1 | _ <- y]))
evalArithmetic env (HString  x) op    (Length   (HString y)) = getVar env x >>= (\a -> evalVal env (Length (HString y)) >>= (\b -> evalArithmetic env a op b))
evalArithmetic env (HInteger x) op    (Length   (HString y)) = evalVal env (Length (HString y)) >>= \length -> evalArithmetic env (HInteger x) op length
--evalArithmetic env (Arith (HList xs) Dot (HInteger ys)) op (HInteger y) = evalArithmetic env (HList xs) Dot (HInteger ys) >>= \i -> evalArithmetic env i op (HInteger y) 

evalVal :: Env -> HVal -> IOThrowsError HVal
evalVal env val@(HInteger _)      = return $ val
evalVal env val@(HBool    _)      = return $ val
evalVal env (HString  var)         = getVar env var >>= \val -> evalVal env val
evalVal env val@(HValString _)     = return $ val
evalVal env val@(HList    _)      = return $ val
evalVal env (Length (HList val))   = return $ HInteger $ sum [ 1 | _ <- val]
evalVal env (Length (HString val)) = getVar env val >>= \list -> evalVal env (Length list) 
evalVal env (Arith x op y)         = evalArithmetic env x op y
evalVal env (Assign var val)       = evalVal env val >>= defineVar env var

----- List Evaluations : Index Accessing, Cons, Car, Cdr and their variations -----
evalVal env (ListAccess (HList xs) (HInteger y))   = return $ xs !! fromIntegral(y)
evalVal env (Cons (HList list1) (HList list2))     = return $ HList $ list1 ++ list2
evalVal env (Cons (HString list1) (HList list2))   = getVar env list1 >>= \list -> evalVal env (Cons list (HList list2))
evalVal env (Cons (HList list1) (HString list2))   = getVar env list2 >>= \list -> evalVal env (Cons (HList list1) list)
evalVal env (Cons (HString list1) (HString list2)) = getVar env list1 >>= \list -> evalVal env (Cons list (HString list2))
evalVal env (Car  (HList (x:xs)))                  = return $ x
evalVal env (Car  (HString list))                  = getVar env list >>= \list' -> evalVal env (Car list')
evalVal env (Cdr  (HList (x:xs)))                  = return $ HList $ xs
evalVal env (Cdr  (HString list))                  = getVar env list >>= \list' -> evalVal env (Cdr list')


evalStatement_ :: Env -> HStatement -> IOThrowsError ()
evalStatement_ env (Do cond expr) = evalVal env cond >>= \x -> case x of
                                                                 HBool False -> return ()
                                                                 HBool True  -> do
                                                                         traverse_ (evalStatement_ env) expr
                                                                         evalStatement_ env (Do cond expr)

evalStatement_ env (Skip skip) = return () 
evalStatement_ env (Print (HString val)) = getVar env val >>= \x -> liftIO $ putStrLn $ show x
evalStatement_ env (Print val) = evalVal env val >>= \x -> liftIO $ putStrLn $ unwords $ words $ show x
evalStatement_ env (Eval val) = do
    result <- evalVal env val
    return ()
evalStatement_ env (If (cond, expr)) = evalVal env cond >>= \x -> case x of
                                                                     HBool False -> return ()
                                                                     HBool True  -> do
                                                                             traverse_ (evalStatement_ env) expr
                                                                             return ()

evalStatement_ env (Ifs if_ ((If (cond, expr)) : ifs) fi_ n) = evalVal env cond >>= \x -> case x of
                                                                                            HBool False -> evalStatement_ env (Ifs if_ ( head ifs : tail ifs) fi_ n)
                                                                                            HBool True  -> traverse_ (evalStatement_ env) expr
-- Non Dterminism                                                                     
evalStatement_ env (Selection if_ selection fi_ n) = evalS env (Selection if_ selection fi_ n) (selection !! randIdx n) 
-- Determinism: traverse_ (evalStatement_ env) selection
-- THIS COMPOSES ALL IFS INTO A SINGLE COMPUTATION !! evalStatement_ env (Ifs if_ ifs fi_ n) = traverse_ (evalStatement_ env) ifs
-- HELPER FUNCTION FOR SELECTION EVALUATION
evalS env (Selection if_ selection fi_ n) (If (cond, expr)) = evalVal env cond >>= \x -> case x of
                                                           HBool False -> evalStatement_ env (Selection if_ selection fi_ n)
                                                           HBool True  -> evalStatement_ env (If (cond, expr)) 

randIdx :: Int -> Int
randIdx n = unsafePerformIO (getStdRandom (randomR (0, n - 1)))

unravel :: [HVal] -> String
unravel list = unwords (map showVal list)


--------------------------------------------------------------
        
        -- ERROR HANDLING AND VARIABLE ASSIGNMENT WAS BASED ON THE WIKIBOOK WRITE YOURSELF A SCHEME IN 48 HOURS in order to faciliate Assignment --
        -- https://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf --

-------------------------------------------------------------

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
showError (UnboundVar stmt var)         = stmt ++ ":" ++ var
showError (BadSpecialForm stmt form)    = stmt ++ ":" ++ show form
showError (NotFunction stmt func)       = stmt ++ ":" ++ show func
showError (NumArgs expected found)      = "Expected:" ++ show expected  ++ "\tFound:" ++ unravel found
showError (TypeMismatch expected found) = "Tyoe Mismatch : Expected:" ++ expected ++ "\tFound:" ++ show found
showError (Parser parseErr)             = "Parsing Error:" ++ show parseErr
showError (Statement statement)         = "Error found in:" ++ show statement

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
                         maybe (throwError $ UnboundVar "Unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> HVal -> IOThrowsError HVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting unbound variable" var)
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


