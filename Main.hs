module Main where

import HParser
import Expr

import Data.Char
import System.IO
import Data.IORef
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding ( spacess )
import System.Environment
import Prelude hiding (head, tail)
import Control.Applicative hiding ((<|>), many)
import Text.Parsec hiding ((<|>))



----------- REPL ----------
-- https://github.com/joelchelliah/simple-repl-in-haskell
--read' :: IO String
--read' = putStr "Olivia>" >> hFlush stdout >> getLine

--eval' :: String -> String
--eval' input = readExpr input

--print' :: String -> IO ()
--print' = putStrLn

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
 result <- prompt
 if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Olivia> ") evalAndPrint

main :: IO ()
main = do
 args <- getArgs
 case length args of
    0 -> runRepl
    1 -> evalAndPrint $ args !! 0
    otherwise -> putStrLn "Program takes only 0 or 1 arguements"

readExpr :: String -> ThrowsError HVal
readExpr input = case parse parseProgram "Olivia" input of
   Left err -> return $ HString $ "Error: " ++ show err
   Right val -> return $ val

--readExpr :: String -> String
--readExpr input = case parse parseProgram "H" input of
--   Left err  -> show err --   Right val -> show val
parseFile :: String -> IO HVal
parseFile file =
	do program <- readFile file
    	   case parse parseProgram "" program of
	  	Left  err    -> fail (show err)
		Right parsed -> return $ parsed

  





