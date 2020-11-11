module Main where

import Parser
import Expr hiding (Env, nullEnv)

import Data.Char
import System.IO
import Data.IORef
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding ( spacess, try )
import System.Environment
import Prelude hiding (head, tail)
import Control.Applicative hiding ((<|>), many)
import Text.Parsec hiding ((<|>))


type Env = IORef [(String, IORef HVal)]

nullEnv :: IO Env
nullEnv = newIORef []

----------- REPL ----------
-- https://github.com/joelchelliah/simple-repl-in-haskell
--read' :: IO String
--read' = putStr "Olivia>" >> hFlush stdout >> getLine

--eval' :: Env -> String -> ThrowsError ()
--eval' env input = evalStatement env

eval'' :: String -> ThrowsError HVal
eval'' input = readHVal input
--print' :: String -> IO ()
--print' = putStrLn

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalStatementString :: Env -> String -> IO String
evalStatementString env expr = runIOThrows $ liftM show $ (liftThrows $ readStatement expr) >>= evalStatement env

evalHValString :: Env -> String -> IO String
evalHValString env expr = runIOThrows $ liftM show $ (liftThrows $ readHVal expr) >>= evalHVal env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalStatementString env expr >>= putStrLn

evalAndPrint' :: Env -> String -> IO ()
evalAndPrint' env expr = evalHValString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
 result <- prompt
 if pred result
    then return ()
    else action result >> until_ pred prompt action

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint' expr

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Olivia> ") . evalAndPrint'

main :: IO HVal
main = do
 args <- getArgs
 case length args of
   -- 0 -> runRepl
    1 -> do
            parseFile (args !! 0)
   -- otherwise -> putStrLn "Program takes only 0 or 1 arguements"

--readStatement :: String -> ThrowsError HVal
--readStatement input = case parse parseExpression "Olivia" input of
--   Left err -> return $ HString $ "Error: " ++ show err
--   Right val -> return $ val

readStatement :: String -> ThrowsError Statement
readStatement input = case parse parseExpression "Olivia" input of
   Right val -> return $ val

readHVal :: String -> ThrowsError HVal
readHVal input = case parse parseHVal "Olivia" input of
                   Left  err -> return $ HString $ "Error: " ++ show err
                   Right val -> return $ val

readLines :: FilePath -> IO [String]
readLines  =  fmap lines . readFile


--main :: IO ()
--main = do
--        expr   <- getArgs
--        evaled <- return $ liftM show $ readStatement (expr !! 0) >>= evalHVal
--        putStrLn $ extractValue $ trapError evaled
parseFile :: String -> IO HVal
parseFile file =  
	do program <- readFile file
    	   case parse parseHVal "" program of
	  	Left  err    -> fail (show err)
                Right parsed -> do
                        a <- lines program
                        return $ parsed 
                        --return $ parsed







