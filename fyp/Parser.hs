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
   deriving (Eq, Read)

data HStatement
  =  Eval HVal deriving (Eq, Read)

data Op = Add | Sub | Mult | Div | Mod | And | Or | Min | Max deriving (Show, Eq, Read)


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
parseOp = classifyOps <$> ( (string "min") <|> (string "max") <|> (string "and") <|> (string "or") <|> (string "+") <|> (string "-") <|> (string "*") <|> (string "div") <|> (string "mod"))
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


parseArith :: Parser HVal
parseArith = do
        x  <- try (parseInteger) <|> try (parseBool)
        spaces
        op <- parseOp
        spaces
        y  <- try (parseInteger) <|> try (parseBool) <|> try (char '(' *> parseArith <* char ')')
        return $ Arith x op y
     <|>
        do
           op <- try (string "min.") <|> try (string "max.")
           x  <- try (parseInteger)
           _  <- string "."
           y  <- try (parseInteger) <|> try (char '(' *> parseArith <* char ')') 
           spaces
           if op == "min." then return $ Arith x Min y else return $ Arith x Max y 

parseEvalHVal :: Parser HStatement
parseEvalHVal = do
        x <- parseArith
        return $ Eval x

parseVals :: Parser HVal
parseVals = try (parseArith) <|> try (parseList) <|> try (parseBool) <|> try (parseString)  <|> try (parseInteger)
