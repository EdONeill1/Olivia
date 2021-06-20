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
  | HValString String
  | HList    [HVal]
  | Length   HVal
  | Arith    HVal Op HVal
  | Assign   String HVal
  | ListAccess HVal HVal
  | Cons     HVal   HVal
  | Car      HVal
  | Cdr      HVal
    deriving (Eq, Read)


data HStatement
  =  Eval   HVal
  |  Print  HVal
  |  Do     HVal [HStatement]
  |  If     (HVal, [HStatement]) 
  |  Selection String [HStatement] String Int
  |  Ifs  String [HStatement] String Int
  |  S [(HVal, [HStatement])]
  |  Skip   String
  |  HFunction String HVal HStatement
    deriving (Eq, Read)

data Op = Add | Sub | Mult | Div | Mod | And | Or | Less | Greater | Leq | Geq | Dot | Equals | NEquals | Up | Down deriving (Show, Eq, Read)

---------- HFunction Parser ----------
-- A function is denoted by it's name, a list of params, and then a list of HStatements which operate upon the params
parseHFunction :: Parser HStatement
parseHFunction = do
       _        <- try (string "def")
       spaces
       funcName <- many letter
       spaces
       _        <- try (string "(")
       params   <- parseList
       _        <- try (string ")->")
       spaces
       funcBody <-  parseEvalHVal
       spaces
       _        <- try (string "end")
       return $ HFunction funcName params funcBody


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

parseValString :: Parser HVal
parseValString = liftM HValString $ (char '"' *> many (noneOf "\"")  <* char '"')

parseList :: Parser HVal
parseList = liftM HList $ (char '[' *> sepBy parseVals spaces <* char ']')


parseOp :: Parser Op
parseOp = classifyOps <$> ( (string "up") <|> (string "down") <|> (string "and") <|> (string "or") <|> (string "+") <|> (string "-") <|> (string "*") <|> (string "/") <|> (string "%") <|> (string "<") <|> (string ">") <|> (string "leq") <|>  (string "geq") <|> (string ".") <|> (string "=") <|> (string "!="))
  where
    classifyOps "up"   = Up
    classifyOps "down" = Down
    classifyOps "and" = And
    classifyOps "or"  = Or
    classifyOps "+"   = Add
    classifyOps "-"   = Sub
    classifyOps "*"   = Mult
    classifyOps "/" = Div
    classifyOps "%" = Mod
    classifyOps "<"   = Less
    classifyOps ">"   = Greater
    classifyOps "leq"  = Leq
    classifyOps "geq"  = Geq
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

parseLength :: Parser HVal -- Parse array length
parseLength = do
        string "len("
        x <- try (parseList) <|> try (parseString)
        string ")"
        spaces
        return $ Length x

parseCons :: Parser HVal
parseCons = do
        string "cons"
        spaces
        list1 <- try (parseList) <|> try (parseString)
        spaces
        list2 <- try (parseList) <|> try (parseString)
        spaces
        return $ Cons list1 list2

parseCdr :: Parser HVal
parseCdr = do
        func <- try (string "cdr")
        spaces
        list <- try (parseList) <|> try (parseString)
        spaces
        return $ Cdr list

parseCar :: Parser HVal
parseCar = do
        func <- try (string "car")
        spaces
        list <- try (parseList) <|> try (parseString)
        spaces
        return $ Car list 
    
parseVals :: Parser HVal -- General parser for HVals
parseVals = try (parseValString) <|> try (parseCons) <|> try (parseCar) <|> try (parseCdr) <|> try (parseListAccess) <|> try (parseLength) <|> try (parseAssign) <|> try (parseArith) <|> try (parseList) <|> try (parseBool) <|> try (parseString) <|> try (parseInteger) 

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


parseEvalHVal :: Parser HStatement -- Parser that bridges gap between HVals and HStatements for HStatement operations
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
     p <- try (parseArith) <|> try (parseBool)
     _ <- string ")->"
     spaces
     q <- many1 $ spaces *> parseStatements
     _ <- string "Od"
     return $ Do p q

parseSkip :: Parser HStatement
parseSkip = do
        skip <- try (spaces *> string "skip" <* spaces)
        return $ Skip skip

parseIf :: Parser HStatement 
parseIf = do
        string "("
        cond  <- parseArith
        string ")->"
        spaces
        expr  <- many1 $ parseStatements 
        spaces
        return $ If (cond, expr) 

--- Testing to incoporate Selection and If parsing together
parseS :: Parser [HStatement]
parseS = many (do
        spaces
        x <- parseVals
        spaces
        y <- many1 $ parseStatements
        spaces
        return $ S [(x, y)])

parseSelection :: Parser HStatement
parseSelection = do
        if_ <- string "select"
        spaces
        selection <- many1 $ parseIf
        spaces
        fi_ <- string "end"
        spaces
        return $ Selection if_ selection fi_ (length selection)

parseIfs :: Parser HStatement
parseIfs = do
        if_ <- string "if"
        spaces
        ifs <- many1 $ parseIf
        spaces
        fi_ <- string "fi"
        spaces
        return $ Ifs if_ ifs fi_ (length ifs)

--- General statement parser
parseStatements :: Parser HStatement
parseStatements = try (parseHFunction) <|> try (parsePrint) <|> try (parseEvalHVal) <|> try (parseDo) <|> try (parseSelection) <|> try (parseIfs) <|> try (parseSkip)








