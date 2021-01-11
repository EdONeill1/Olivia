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
  | Arith    HVal Op HVal
  |  Assign String HVal
    deriving (Eq, Read)

data HStatement
  =  Eval   HVal
  |  Print  HVal
  |  Do     HVal [HVal]
    deriving (Eq, Read)

data Op = Add | Sub | Mult | Div | Mod | And | Or | Min | Max | Less | Greater deriving (Show, Eq, Read)


---------- HVal Parsers ----------

parseInteger :: Parser HVal
parseInteger = many1 digit >>= (return . HInteger . read)


parseBool :: Parser HVal
parseBool = f <$> (string "True" <|> string "False")
  where
    f "True"  = HBool True
    f "False" = HBool False


parseString :: Parser HVal
parseString = many1 (letter) >>= (return . HString)


parseList :: Parser HVal
parseList = liftM HList $ (char '[' *> sepBy parseVals spaces <* char ']')

parseOp :: Parser Op
parseOp = classifyOps <$> ( (string "min") <|> (string "max") <|> (string "and") <|> (string "or") <|> (string "+") <|> (string "-") <|> (string "*") <|> (string "div") <|> (string "mod") <|> (string "<") <|> (string ">"))
  where
    classifyOps "min" = Min
    classifyOps "max" = Max
    classifyOps "and" = And
    classifyOps "or"  = Or
    classifyOps "+"   = Add
    classifyOps "-"   = Sub
    classifyOps "*"   = Mult
    classifyOps "div" = Div
    classifyOps "mod" = Mod
    classifyOps "<"   = Less
    classifyOps ">"   = Greater

parseArith :: Parser HVal
parseArith = do
        x  <- try (parseInteger) <|> try (parseBool) <|> try (parseString)
        spaces
        op <- parseOp
        spaces
        y  <- try (parseInteger) <|> try (parseBool) <|> try (parseString) <|> try (char '(' *> parseArith <* char ')')
        return $ Arith x op y
     <|>
        do
           op <- try (string "min.") <|> try (string "max.")
           x  <- try (parseInteger)
           _  <- string "."
           y  <- try (parseInteger) <|> try (char '(' *> parseArith <* char ')') 
           spaces
           if op == "min." then return $ Arith x Min y else return $ Arith x Max y 


parseVals :: Parser HVal
parseVals = try (parseAssign) <|> try (parseArith) <|> try (parseList) <|> try (parseBool) <|> try (parseString)  <|> try (parseInteger)

---------- Statement Parsers ----------

parseAssign :: Parser HVal
parseAssign = do
        var <- many letter
        spaces
        _   <- string ":="
        spaces
        val <- try (parseVals) <|> try (parseArith)
        return $ Assign var val

parseEvalHVal :: Parser HStatement
parseEvalHVal = do
        x <- try (parseVals)
        return $ Eval x


parsePrint :: Parser HStatement
parsePrint = do
        _ <- try (string "print")
        _ <- char '('
        toPrint <- parseVals 
        _ <- char ')'
        return $ Print toPrint




parseDo :: Parser HStatement
parseDo = do
   string "Do"
   spaces
   char '('
   cond  <- try (parseArith)
   string ")->"
   spaces
   expr  <- many (parseVals)
   spaces
   string "Od" 
   return $ Do cond expr

parseStatements :: Parser HStatement
parseStatements = try (parsePrint) <|> try (parseEvalHVal)









