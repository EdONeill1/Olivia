module Main where

import Parser
import Expr

import System.Environment
import Text.ParserCombinators.Parsec

readExpr :: String -> HVal
readExpr input = case parse parseVals "fyp" input of
                   Left err  -> HString $ "No match: " ++ show err
                   Right val -> val


--main :: IO()
--main = getArgs >>= putStrLn . show . evalVal . readExpr . (!! 0) 

main :: IO ()
main = do
        args <- getArgs
        parsed <- parseFile (args !! 0)
        putStrLn $ show $ evalStatement parsed

parseFile :: String -> IO HStatement
parseFile file =
	do program <- readFile file
    	   case parse parseEvalHVal "" program of
	  	Left  err    -> fail (show err)
		Right parsed -> return $ parsed



