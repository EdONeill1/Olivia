module Main where

import Parser
import Expr

import System.Environment
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.List
import Control.Concurrent.Async
import Data.Traversable


readStatement :: String -> IO [HStatement]
readStatement input = do
        program <- readFile input
        case parse parseProgram "Olivia" program of
          Left err -> fail $ show err
          Right parsed -> return $ parsed


evalString :: Env -> String -> IO String
evalString env expr = do
        x <- readStatement expr
--        putStrLn $ show x
        concat <$> traverse (runIOThrows . liftM show . evalStatement_ env) x


evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = do
        evalString env expr
        return ()
                          
run :: String -> IO ()
run expr = nullEnv >>= flip evalAndPrint expr

main :: IO ()
main = do
        args <- getArgs
        run $ args !! 0

parseProgram :: Parser [HStatement]
parseProgram = spaces *> many (parseStatements <* spaces)
