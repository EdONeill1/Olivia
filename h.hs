import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.Environment


-- data BExpr = Bool Bool
--            | Not BExpr
--            | BBinary BBinOp BExpr BExpr
--            | RBinary RBinOp AExpr AExpr
--             deriving (Show)

data BBinOp = And | Or deriving (Show)
data RBinOp = Greater | Less deriving (Show)

data HenryVal = String
              | Integer Integer
              | Bool Bool
              | Not HenryVal
              | List [HenryVal] 
              | Neg HenryVal
              | ABinary ABinOp HenryVal HenryVal
              | BBinary BBinOp HenryVal HenryVal
              | RBinary RBinOp HenryVal HenryVal
              | Seq [HenryVal]
              | Assign String HenryVal
              | If HenryVal HenryVal HenryVal
              | While HenryVal HenryVal
              | Skip
                deriving (Show)

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


spaces :: Parser ()
spaces = skipMany1 space

ifStmt :: Parser HenryVal
ifStmt =
  do reserved "if"
     cond  <- bExpression
     reserved "->"
     stmt1 <- statement
     reserved "if"
     stmt2 <- statement
     reserved "->"
     return $ If cond stmt1 stmt2

whileStmt :: Parser HenryVal
whileStmt =
  do reserved ";Do"
     cond <- bExpression
     reserved "->"
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


parseFile :: String -> IO HenryVal
parseFile file =
  do program  <- readFile file
     case parse henryParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r



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


readExpr :: String -> HenryVal
readExpr input = case parse henryParser "Henry" input of
    Left err -> String $ "No Match" ++ show err
    Right val -> String $ val

--- Evalutation
showVal :: HenryVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Integer contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

eval :: HenryVal -> HenryVal
eval val@(String _) = val
eval val@(Integer _) = val
eval val@(Bool _) = val


main :: IO ()
main = getArgs >>= print . eval . readExpr . head
-- main :: IO ()
-- main = do 
--          (expr:_) <- getArgs
--          putStrLn (readExpr expr)