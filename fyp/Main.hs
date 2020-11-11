module Main where

import Parser
import Expr

import System.Environment
import Text.ParserCombinators.Parsec

readExpr :: String -> HVal
readExpr input = case parse parseVals "fyp" input of
                   Left err  -> HString $ "No match: " ++ show err
                   Right val -> val


main :: IO()
main = getArgs >>= putStrLn . show . evalVal . readExpr . (!! 0) 

