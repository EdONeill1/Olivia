import System.IO
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.Environment

read' :: IO String
read' = putStr "REPL> "
     >> hFlush stdout
     >> getLine

eval' :: String -> String
eval' input = input

print' :: String -> IO ()
print' = putStrLn

main :: IO ()
main = do
  input <- read'
  
  unless (input == ":quit")
       $ print' (eval' input) >> main