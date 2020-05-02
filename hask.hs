import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.Environment
instance Show HenryVal where show = showVal

--- Definition of data, language, and lexer
data HenryVal = Atom String
              | String String 
              | Integer Integer
              | Bool Bool
              | Not HenryVal
              | Neg HenryVal
              | List [HenryVal]
              | Seq [HenryVal]
              | Assign String HenryVal
              | If HenryVal HenryVal HenryVal
              | While HenryVal HenryVal
              | Skip
              | ABinary ABinOp HenryVal HenryVal
              | BBinary BBinOp HenryVal HenryVal
              | RBinary RBinOp HenryVal HenryVal

data BBinOp = And | Or deriving (Show)
data RBinOp = Greater | Less deriving (Show)
data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
              deriving (Show)

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "od"
                                     , "skip"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     , "->"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                      , "<", ">", "and", "or", "not"
                                     ]
           }
lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

----------

--- Definition of Parsers
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
                         "True" -> Bool True
                         "False" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser HenryVal
parseNumber = liftM (Integer . read) $ many1 digit

parseList :: Parser HenryVal
parseList = liftM List $ sepBy parseExpr spaces

statement :: Parser HenryVal
statement =   parens statement
          <|> sequenceOfStmt

sequenceOfStmt =
  do list <- (sepBy1 statement' semi)
     return $ if length list == 1 then head list else Seq list


henryParser :: Parser HenryVal
henryParser = whiteSpace >> statement


statement' :: Parser HenryVal
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> assignStmt

parseExpr :: Parser HenryVal   
parseExpr =   henryParser
          <|> parseAtom
          <|> parseNumber 
          <|> parseString
          <|> do 
                _ <- char '['
                x <- try parseList
                _ <- char ']'
                return x

ifStmt :: Parser HenryVal
ifStmt =
  do reserved "if"
     cond  <- bExpression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ If cond stmt1 stmt2
  <|>
  do reserved "if"
     cond  <- rExpression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ If cond stmt1 stmt2


whileStmt :: Parser HenryVal
whileStmt =
  do reserved ";Do"
     cond <- bExpression
     reserved "while"
     stmt <- statement
     reserved "Od"
     return $ While cond stmt

assignStmt :: Parser HenryVal
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Assign var expr

skipStmt :: Parser HenryVal
skipStmt = reserved "skip" >> return Skip

aExpression :: Parser HenryVal
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser HenryVal
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
             , [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft,
                Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft]
              ]

bOperators = [ [Prefix (reservedOp "not" >> return (Not             ))          ]
             , [Infix  (reservedOp "and" >> return (BBinary And     )) AssocLeft,
                Infix  (reservedOp "or"  >> return (BBinary Or      )) AssocLeft]
             ]
            
aTerm =  parens aExpression
     <|> liftM String identifier
     <|> liftM Integer integer

   
bTerm =  parens bExpression <|> (reserved "true"  >> return (Bool True )) <|> (reserved "false" >> return (Bool False)) <|> rExpression

rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ RBinary op a1 a2

relation = (reservedOp ">" >> return Greater) <|> (reservedOp "<" >> return Less)


parseFile :: String -> IO HenryVal
parseFile file =
  do program  <- readFile file
     case parse henryParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

--- Beginning of evaluation
showVal :: HenryVal -> String
showVal (Atom contents) = show contents
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Integer contents) = show contents
showVal (Neg contents) = "-" ++ show contents
showVal (Bool True) = "True"
showVal (Bool False) = "False"
showVal (Not contents) = "Not " ++ show contents
showVal (List contents) = "[" ++ unwordsList contents ++ "]"
showVal (Seq contents) = unwordsList contents
showVal (Assign val v) = val ++ " := " ++ show v
showVal (If cond stmt1 stmt2) = "if " ++ show cond ++ " then " ++ show stmt1 ++ "else " ++ show stmt2 
showVal (While cond stmt) = ";Do" ++ show cond ++ "while" ++ show stmt ++ "Od"
showVal (Skip) = "Skip"
showVal (ABinary op x y) = show x ++ show op ++ show y 
showVal (BBinary op x y) = show x ++ show op ++ show y 
showVal (RBinary op x y) = show x ++ show op ++ show y 

unwordsList :: [HenryVal] -> String
unwordsList = unwords . map showVal

-- eval :: HenryVal -> HenryVal
-- eval val@(String _) = val
-- eval val@(Integer _) = val
-- eval val@(Bool _) = val
-- eval (List [Atom "quote", val]) = val

readExpr :: String -> String
readExpr input = case parse parseExpr "Henry" input of
    Left err -> "No match: " ++ show err
    Right val -> show val

main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn (readExpr expr)


