module HParser where

import Data.Char
import System.IO
import Data.IORef
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding ( spacess )
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
  | Expr    HVal Op HVal  -- Expressions will be thought of as data. There will be one parser that parsers these 
  | VarExpr String Op HVal
  | EqExpr  HVal Op HVal  -- Redundacy in grammar, will have to fix
 | Neg      HVal
  | Assign  String HVal  -- expressions and there will be another to parse the 'atomic' data.
  | Do	    HVal  HVal 
  | If      HVal  [HVal] HVal
  | SubIf   HVal  [HVal]
  | Load    String
  | Program  [HVal]
  deriving (Eq, Read)

data Op
  = Add
  | Sub
  | Mult
  | Div
  | Mod
  | And
  | Or
  | Greater
  | Less
  | GreaterEq
  | LessEq
  | Not
  | Equal
  deriving (Show, Eq, Read)

---------- EXPRESSION PARSERS ----------

load :: Parser HVal
load = do
  string "load"
  spaces
  x <- many (letter <|> space)
  return $ Load x

parseDo :: Parser HVal
parseDo = do
   _     <- char '('
   cond  <- (parseIf <|> parseBool <|> parseExpr <|> parseEqExpr)  -- Will be changed to a Boolean expression
   _     <- string ")->"
   spaces
   expr  <- parseExpression --parseProgram 
   spaces
    --_  <- string "Od " Program doesn't parse properly when Od is included.
   return $ Do cond expr
   

parseIf :: Parser HVal
parseIf = do
 _     <-  string "("
 cond  <- (parseExpr <|> parseEqExpr <|> parseBool) 
 _     <-  string ")->"
 expr  <- spaces *> many (parseExpression <* spaces)
 expr' <- parseSubIf
 return $ If cond expr expr'

parseSubIf :: Parser HVal
parseSubIf = do
  _ <- string "[]"
  spaces
  _ <- string "("
  cond <- (parseExpr <|> parseEqExpr <|> parseBool) 
  _ <- string ")->"
  expr <- spaces *> many (parseExpression <* spaces)
  return $ SubIf cond expr

parseEqExpr :: Parser HVal
parseEqExpr =
 do
   _  <- char '('
   x  <- parseExpr
   _  <- char ')'
   spaces
   op <- parseOp  -- ==
   spaces
   _  <- char '('
   y  <- parseExpr <|> parseInteger
   _  <- char ')'
   return $ EqExpr x op y

parseVarExpr :: Parser HVal
parseVarExpr = do
   x  <- many (letter)
   spaces
   op <- parseOp
   spaces
   y  <- parseInteger <|> parseExpr
   return $ VarExpr x op y

parseExpr :: Parser HVal
parseExpr = do
   x  <- (parseBool <|> parseInteger <|> parseString)
   spaces 
   op <- parseOp
   spaces
   y  <- parseBool <|> parseInteger <|> parseString <|> do
                                                          char '('
                                                          z <- parseExpr
                                                          char ')'
                                                          return $ z
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


parseNot :: Parser HVal
parseNot = do
  _ <- string "not"
  spaces
  x <- parseBool <|> do
  			_ <- char '('
			z <- parseExpr 
			_ <- char ')'
			return $ z
  return $ Neg x


parseExpression :: Parser HVal
parseExpression = (string "Do" *> spaces *> parseDo) <|> (string "if" *> spaces *> parseIf) <|> parseAssign <|> parseExpr

parseProgram :: Parser HVal
parseProgram = do
                  x <- spaces *> many (parseExpression <* spaces)
                  spaces
                  return $ Program x



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
parseHVal = parseInteger <|> parseBool <|> parseString <|> parseList 




