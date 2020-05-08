
import System.IO
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.Environment
instance Show HenryVal where show = showVal

data HenryVal = Integer Integer
              | Bool Bool
              | Atom String
              | String String 
              | List [HenryVal]
              | ABinOp ABinOp
              | ABinary ABinOp HenryVal HenryVal

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
             deriving (Show)

spaces :: Parser ()
spaces = skipMany1 space

parseABinOp :: Parser HenryVal
parseABinOp = do 
              first <- letter
              rest <- many letter
              let atom = first:rest
              return $ case atom of 
                         "Add" -> ABinOp Add
                         "Subtract" -> ABinOp Subtract
                         "Multiply" -> ABinOp Multiply
                         "Divide" -> ABinOp Divide
                       

--   x <- parseNumber
--     op <- parseABinOp
--     y <- parseNumber
--     return $ ABinary Add x y



parseABinary :: Parser HenryVal
parseABinary = do
                x <- parseNumber
                op <- oneOf "/*+-"
                y <- parseNumber
                if op == '*' 
                    then 
                        return $ ABinary Multiply x y 
                else
                    if op == '/' then
                        return $ ABinary Divide x y 
                    else 
                        if op == '+' then
                            return $ ABinary Add x y
                        else 
                            if op == '-' then 
                                return $ ABinary Subtract x y
                            else
                                return $ String "Error"
              


  

parseString :: Parser HenryVal
parseString = do
    _ <- char '"'
    x <- many (noneOf "\"")
    _ <- char '"'
    return $ String x

parseList :: Parser HenryVal
parseList = liftM List $ sepBy parseExpr spaces

parseNumber :: Parser HenryVal
parseNumber = liftM (Integer . read) $ many1 digit

parseAtom :: Parser HenryVal
parseAtom = do 
              first <- letter
              rest <- many (letter <|> digit)
              let atom = first:rest
              return $ case atom of 
                         "True" -> Bool True
                         "False" -> Bool False
                         "Add" -> ABinOp Add
                         "Subtract" -> ABinOp Subtract
                         "Multiply" -> ABinOp Multiply
                         "Divide" -> ABinOp Divide
                         _ -> Atom atom

--  parseABinOp <|>
parseExpr :: Parser HenryVal   
parseExpr =  parseNumber <|>
             parseAtom   <|>
             parseString <|>
             do
                 _ <- char '['
                 x <- try parseList
                 _ <- char ']'
                 return x
            <|>
            do
                 _ <- char '<'
                 x <- try parseABinary
                 _ <- char '>'
                 return x

readExpr :: String -> HenryVal
readExpr input = case parse parseExpr "Henry" input of
    Left error -> String $ "No Match: " ++ show error
    Right val -> val

showVal :: HenryVal -> String
showVal (Integer contents) = show contents
showVal (Atom contents) = show contents
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Bool True) = "True"
showVal (Bool False) = "False"
showVal (ABinOp op) = show op
showVal (ABinary op x y) = show x ++ " " ++ show op ++ " " ++ show y 
showVal (List contents) = "[" ++ unwordsList contents ++ "]"

unwordsList :: [HenryVal] -> String
unwordsList = unwords . map showVal

evalABinOp :: HenryVal -> ABinOp -> HenryVal -> HenryVal
evalABinOp (Integer a) Add (Integer b) = Integer (a +b)
evalABinOp (Integer a) Multiply (Integer b) = Integer (a * b)
evalABinOp (Integer a) Divide (Integer b) = Integer (a `div` b)
evalABinOp (Integer a) Subtract (Integer b) = Integer (a - b) 

eval :: HenryVal -> HenryVal
eval val@(Atom _) = val
eval val@(String _) = val
eval val@(Integer _) = val
eval val@(Bool _) = val
eval val@(ABinOp _) = val
eval (List [Atom "quote", val]) = val
eval val@(List _) = val
eval (ABinary op x y) = evalABinOp (eval x) op (eval y)

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
-- main :: IO ()
-- main = do 
--          (expr:_) <- getArgs
--          putStrLn (readExpr expr)


