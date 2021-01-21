module Main where

import Parser
import Expr

import System.Environment
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.List

readStatement :: String -> IO [HStatement]
readStatement input = do
        program <- readFile input
        case parse parseProgram "Oli" program of
          Left err -> fail $ show err
          Right parsed -> return $ parsed


evalString :: Env -> String -> IO String
evalString env expr = do
        x <- readStatement expr
        concat <$> mapM (runIOThrows . liftM show . evalStatement_ env) x

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = do
        evalString env expr
        return ()
                          

run :: String -> IO ()
run expr = nullEnv >>= flip evalAndPrint expr

main :: IO ()
main = do
        args   <- getArgs
        run $ args !! 0

parseFile :: String -> IO [HStatement]
parseFile file = do 
        program <- readFile file
        case parse parseProgram "Oli" program of 
          Left  err    -> fail (show err)
          Right parsed -> return $ parsed

parseProgram :: Parser [HStatement]
parseProgram = spaces *> many (parseStatements <* spaces)

