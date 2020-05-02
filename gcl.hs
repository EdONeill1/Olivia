import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.Environment


data BExpr = Bool Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
            deriving (Show)

data BBinOp = And | Or deriving (Show)
data RBinOp = Greater | Less deriving (Show)

data AExpr = Var String
           | Integer Integer
           | List [AExpr]
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
             deriving (Show)

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
              deriving (Show)

data Stmt = Seq [Stmt]
          | Assign String AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
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
brackets = Token.brackets lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement =   brackets statement
          <|> sequenceOfStmt

sequenceOfStmt =
  do list <- (sepBy1 statement' semi)
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> assignStmt


ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond  <- bExpression
     reserved "->"
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

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
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

aTerm =  brackets aExpression
     <|> liftM Var identifier
     <|> liftM Integer integer

bTerm =  brackets bExpression
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
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r


-- data AExpr = Var String
--            | Integer Integer
--            | List [AExpr]
--            | Neg AExpr
--            | ABinary ABinOp AExpr AExpr
--              deriving (Show)

showVal :: AExpr -> String
showVal (Var contents) = "" ++ show contents ++ ""
showVal (Integer contents) = show contents
showVal (List contents) = "[" ++ unwordsList contents ++ "]"

eval :: AExpr -> Stmt
eval val@(Var _) = val
eval val@(Integer _) = val
eval (List [Var "quote", val]) = val


unwordsList :: [Stmt] -> String
unwordsList = unwords . map showVal


readExpr :: String -> Stmt
readExpr input = case parse whileParser "" input of
    Left err -> Var $ "No Match" ++ show err
    Right val -> val


main :: IO ()
main = getArgs >>= print . eval . readExpr . head