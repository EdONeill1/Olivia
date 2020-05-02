import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.Environment


data HenryVal = Integer Integer
              | String String
              | Bool Bool
              | List [HenryVal]

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"


parseExpr :: Parser HenryVal
parseExpr = parseString <|> parseNumber

-- parseString :: Parser HenryVal
-- parseString = do
--                 char '"'
--                 x <- many (noneOf "")
--                 char '"'
--                 return $ String x


parseString :: Parser HenryVal
parseString = do
    char '"'
    x <- many letter
    char '"'
    return $ String x


parseNumber :: Parser HenryVal
parseNumber = liftM (Integer . read) $ many1 digit

main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn (readExpr expr)