module Main where

import HParser

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
import Data.IORef


----------- REPL ----------
-- https://github.com/joelchelliah/simple-repl-in-haskell
read' :: IO String
read' = putStr "Olivia>" >> hFlush stdout >> getLine

eval' :: String -> String
eval' input = readExpr input

print' :: String -> IO ()
print' = putStrLn




readExpr :: String -> String
readExpr input = case parse parseIf "H" input of
   Left err  -> show err
   Right val -> show val


parseFile :: String -> IO [HVal]
parseFile file =
	do program <- readFile file
    	   case parse parseProgram "" program of
	  	Left  err    -> fail (show err)
		Right parsed -> return $ parsed



main :: IO ()
main = do
  input <- read'
  
  unless (input == ":quit")
       $ print' (eval' input) >> main
