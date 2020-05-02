


import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.Environment
instance Show HenryVal where show = showVal

data HenryVal = Atom String
              | String String 
              | Integer Integer
              | Bool Bool
              | List [HenryVal]


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser HenryVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser HenryVal
parseAtom = do 
              first <- letter
              rest <- many (letter <|> digit)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser HenryVal
parseNumber = liftM (Integer . read) $ many1 digit

parseExpr :: Parser HenryVal
parseExpr =   parseAtom
          <|> parseNumber 
          <|> parseString
          <|> do 
                char '['
                x <- try parseList
                char ']'
                return x


readExpr :: String -> String
readExpr input = case parse parseExpr "Henry" input of
    Left err -> "No match: " ++ show err
    Right val -> show val

parseList :: Parser HenryVal
parseList = liftM List $ sepBy parseExpr spaces


showVal :: HenryVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Integer contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "[" ++ unwordsList contents ++ "]"

unwordsList :: [HenryVal] -> String
unwordsList = unwords . map showVal

eval :: HenryVal -> HenryVal
eval val@(String _) = val
eval val@(Integer _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val

-- main :: IO ()
-- main = getArgs >>= print . eval . readExpr . head
main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn (readExpr expr)