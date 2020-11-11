module Parser where

import Data.Char
import System.IO
import Data.IORef
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding ( spacess, try )
import System.Environment
import Prelude hiding (head, tail)
import Control.Applicative hiding ((<|>), many)
import Text.Parsec hiding ((<|>))
import Data.IORef


data HVal
  = HInteger Integer
  | HBool    Bool
  | HString  String
  | HList    [HVal]
  | Neg      HVal
  | Expr     HVal  Op HVal
  | Assign   String HVal 
   deriving (Eq, Read)

data Statement
  = Do	    HVal  [HVal] 
  | Program [HVal] [Statement]
  deriving (Eq, Read)

data Prog
  = Atoms   [HVal]
  | Actions [Statement]
  deriving (Eq, Read)

data Op
  = Add | Sub | Mult | Div | Mod | And | Or | Greater | Less | GreaterEq | LessEq | Not | Equal
  deriving (Show, Eq, Read)

---------- EXPRESSION PARSERS ----------

parseDo :: Parser Statement
parseDo = do
   char '('
   cond  <- parseBool <|> parseExpr
   string ")->"
   spaces
   expr  <- many (parseHVal)
   spaces
   return $ Do cond expr



parseVals :: Parser HVal
parseVals = parseString <|> parseInteger <|> parseBool

parseExpr :: Parser HVal
parseExpr = do 
        x  <- parseVals
        spaces
        op <- parseOp
        spaces
        y  <- try (char '(' *> parseExpr <* char ')') <|> parseVals
        return $ Expr x op y


parseAssign :: Parser HVal 
parseAssign = do
 var <- many letter
 spaces
 _   <- string ":="
 spaces
 val <- parseHVal <|> do 
	 		_ <- char '('
                        z <- parseExpr
			_ <- char ')'
			return $ z

 return $ Assign var val


parseExpression :: Parser Statement
parseExpression = try (string "Do" *> spaces *> parseDo) 

parseProgram :: Parser Statement
parseProgram = do
                  x <- try( spaces *> many (parseHVal <* spaces))
                  y <- try( spaces *> many (parseExpression <* spaces))
                  spaces
                  return $ Program x y



---------- Atomic HVal Parsers ----------

parseOp :: Parser Op
parseOp = ((string "+") <|> (string "-") <|> (string "*") <|> (string "div") <|> (string "mod") <|> (string "and") <|> (string "or") <|> (string ("<")) <|> (string "<=") <|>
	   (string ">") <|> (string ">=") <|> (string "==")) >>= \x -> return $ case x of
 "+"   -> Add
 "-"   -> Sub
 "*"   -> Mult
 "div" -> Div
 "mod" -> Mod
 "and" -> And
 "or"  -> Or
 "<"   -> Less
 "<="  -> LessEq
 ">"   -> Greater
 ">="  -> GreaterEq
 "=="  -> Equal

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
parseList = liftM HList $ (char '[' *> sepBy parseHVal spaces <* char ']')


parseHVal :: Parser HVal
parseHVal = try (parseExpr) <|> try (parseAssign) <|> parseInteger <|> parseBool <|> parseString <|> parseList 

