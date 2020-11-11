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

data Op = Add | Sub | Mult | Div | Mod deriving (Show, Eq, Read)


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
parseOp = classifyOps <$> ((string "+") <|> (string "-") <|> (string "*") <|> (string "div") <|> (string "mod"))
  where
    classifyOps "+"   = Add
    classifyOps "-"   = Sub
    classifyOps "*"   = Mult
    classifyOps "div" = Div
    classifyOps "mod" = Mod


parseArith :: Parser HVal
parseArith = do
        x  <- parseInteger -- Will add string for variables letter
        spaces
        op <- parseOp
        spaces
        y <- try (parseInteger) <|> try (char '(' *> parseArith <* char ')')
        return $ Arith x op y

parseVals :: Parser HVal
parseVals = try (parseArith) <|> try (parseList) <|> try (parseBool) <|> try (parseString)  <|> try (parseInteger)
