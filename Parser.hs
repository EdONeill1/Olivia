module Parser where

import Data.Char
import System.IO
import Data.IORef
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spacess, try)
import System.Environment
import Prelude hiding (head, tail)
import Control.Applicative hiding ((<|>), many)
import Text.Parsec hiding ((<|>))

data HVal
  = HInteger Integer
  | HBool    Bool
  | HString  String
  | HList    [HVal]
  | Length   HVal
  | Arith    HVal Op HVal
  | Assign   String HVal
  | ListAccess HVal HVal
    deriving (Eq, Read)


data HStatement
  =  Eval   HVal
  |  Print  HVal
  |  Do     HVal [HStatement]
  |  If     (HVal, [HStatement]) 
  |  Selection String [HStatement] String
  |  Skip   String
    deriving (Eq, Read)


data Op = Add | Sub | Mult | Div | Mod | And | Or | Min | Max | Less | Greater | Dot | Equals | NEquals deriving (Show, Eq, Read)


---------- HVal Parsers ----------

parseInteger :: Parser HVal
parseInteger = many1 digit >>= (return . HInteger . read)


parseBool :: Parser HVal
parseBool = classifyBool <$> (string "True" <|> string "False")
  where
    classifyBool "True"  = HBool True
    classifyBool "False" = HBool False


parseString :: Parser HVal
parseString = many1 (letter) >>= (return . HString)


parseList :: Parser HVal
parseList = liftM HList $ (char '[' *> sepBy parseVals spaces <* char ']')


parseOp :: Parser Op
parseOp = classifyOps <$> ( (string "min") <|> (string "max") <|> (string "and") <|> (string "or") <|> (string "+") <|> (string "-") <|> (string "*") <|> (string "div") <|> (string "%") <|> (string "<") <|> (string ">") <|> (string ".") <|> (string "=") <|> (string "!="))
  where
    classifyOps "min" = Min
    classifyOps "max" = Max
    classifyOps "and" = And
    classifyOps "or"  = Or
    classifyOps "+"   = Add
    classifyOps "-"   = Sub
    classifyOps "*"   = Mult
    classifyOps "div" = Div
    classifyOps "%" = Mod
    classifyOps "<"   = Less
    classifyOps ">"   = Greater
    classifyOps "."   = Dot
    classifyOps "="   = Equals
    classifyOps "!="  = NEquals


parseListAccess :: Parser HVal
parseListAccess = do
        x <- try (parseList)
        spaces
        string "."
        spaces
        y <- try (parseInteger)
        spaces
        return $ ListAccess x y

parseArith :: Parser HVal
parseArith = do
        x  <-  try (parseListAccess) <|> try (parseLength) <|> try (parseList) <|> try (parseInteger) <|> try (parseBool) <|> try (parseString) <|> try (char '(' *> parseArith <* char ')')
        spaces
        op <- parseOp
        spaces
        y  <- try (parseArith) <|> try (parseLength) <|> try (parseList) <|> try (parseInteger) <|> try (parseBool) <|> try (parseString) <|> try (char '(' *> parseArith <* char ')') 
        spaces
        return $ Arith x op y
     <|>
        do
           op <- try (string "min.") <|> try (string "max.")
           x  <- try (parseInteger)
           _  <- string "."
           y  <- try (parseInteger) <|> try (char '(' *> parseArith <* char ')') 
           spaces
           if op == "min." then return $ Arith x Min y else return $ Arith x Max y 

parseLength :: Parser HVal
parseLength = do
        string "len("
        x <- try (parseList) <|> try (parseString)
        string ")"
        spaces
        return $ Length x


parseVals :: Parser HVal
parseVals = try (parseListAccess) <|> try (parseLength) <|> try (parseAssign) <|> try (parseArith) <|> try (parseList) <|> try (parseBool) <|> try (parseString) <|> try (parseInteger)

p = try (parseAssign) <|> try (parseArith)
---------- Statement Parsers ----------

parseAssign :: Parser HVal
parseAssign = do
        var <- try( many $ try (letter) <|> try (oneOf ['.']) )
        spaces
        _   <- string ":="
        spaces
        val <- try (parseArith) <|> try (parseVals)  
        spaces
        return $ Assign var val


parseEvalHVal :: Parser HStatement
parseEvalHVal = do
        x <- try p
        spaces
        return $ Eval x


parsePrint :: Parser HStatement
parsePrint = do
        _ <- try (string "print")
        _ <- char '('
        toPrint <- parseVals 
        _ <- char ')'
        spaces
        return $ Print toPrint


parseDo :: Parser HStatement
parseDo = do
     _ <- string "Do"
     spaces
     _ <- string "("
     p <- parseArith
     _ <- string ")->"
     spaces
     q <- many1 $ spaces *> parseStatements
     _ <- string "Od"
     return $ Do p q
   --_ <- try (string "Do")
   --spaces
   --_ <- try (string "(")
   --cond  <- try (spaces *> parseVals)
   --_ <- try (string ")->") *> spaces
   --expr  <- many1 $ try (parseSelection <* spaces) <|> (parseEvalHVal) --try (spaces *> many1 parseEvalHVal)
   --_ <- try (string "Od")
   --return $ Do cond expr


parseSkip :: Parser HStatement
parseSkip = do
        skip <- try (spaces *> string "skip" <* spaces)
        return $ Skip skip


parseVal = string "<" *> spaces *> parseStatements <* spaces <* string ">"


parseIf :: Parser HStatement
parseIf = do
        string "("
        cond  <- parseArith
        string ")->"
        spaces
        expr  <- many1 $ parseStatements 
        spaces
        return $ If (cond, expr) 


parseSelection :: Parser HStatement
parseSelection = do
        if_ <- string "if"
        spaces
        selection <- many1 $ parseIf
        spaces
        fi_ <- string "fi"
        spaces
        return $ Selection if_ selection fi_

parseStatements :: Parser HStatement
parseStatements = try (parsePrint) <|> try (parseEvalHVal) <|> try (parseDo) <|> try (parseSelection) 









