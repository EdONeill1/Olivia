

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"




readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"


main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn (readExpr expr)