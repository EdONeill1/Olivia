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
import Data.IORef



----------- REPL ----------
-- https://github.com/joelchelliah/simple-repl-in-haskell
--read' :: IO String
--read' = putStr "Olivia>" >> hFlush stdout >> getLine

--eval' :: String -> String
--eval' input = readExpr input

--print' :: String -> IO ()
--print' = putStrLn

readExpr :: String -> HVal
readExpr input = case parse parseHVal "Olivia" input of
   Left err -> HString $ "Error: " ++ show err
   Right val -> val

--readExpr :: String -> String
--readExpr input = case parse parseProgram "H" input of
--   Left err  -> show err
--   Right val -> show val


parseFile :: String -> IO [HVal]
parseFile file =
	do program <- readFile file
    	   case parse parseProgram "" program of
	  	Left  err    -> fail (show err)
		Right parsed -> return $ parsed

main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!!0)
--main :: IO ()
--main = do
--  input <- read'
  
 -- unless (input == ":quit")
   --    $ print' (eval' input) >> main






