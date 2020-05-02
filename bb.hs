


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

data Stmt = Seq [Stmt]
          | Assign String HenryVal
          | If HenryVal Stmt Stmt
          | While HenryVal Stmt
          | Skip
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



ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond  <- bExpression
     reserved ""
     stmt1 <- statement
     reserved "if"
     stmt2 <- statement
     reserved "->"
     return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt =
  do reserved ";Do"
     cond <- bExpression
     reserved "->"
     stmt <- statement
     reserved "Od"
     return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

aExpression :: Parser Stmt
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser Stmt
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

--   <|> liftM Integer integer
aTerm =  parens aExpression
     <|> liftM String  identifier
     <|> liftM Integer integer

   

bTerm =  parens bExpression
     <|> (reserved "true"  >> return (Bool True ))
     <|> (reserved "false" >> return (Bool False))
     <|> rExpression

rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ RBinary op a1 a2

relation =   (reservedOp ">" >> return Greater)
         <|> (reservedOp "<" >> return Less)


parseFile :: String -> IO Stmt
parseFile file =
  do program  <- readFile file
     case parse henryParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r



statement :: Parser Stmt
statement =   parens statement
          <|> sequenceOfStmt

sequenceOfStmt =
  do list <- (sepBy1 statement' semi)
     return $ if length list == 1 then head list else Seq list


henryParser :: Parser Stmt
henryParser = whiteSpace >> statement


statement' :: Parser Stmt
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> assignStmt

