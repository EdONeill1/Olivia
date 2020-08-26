import Control.Applicative
import Data.Char
---------- H Abstract Syntax Tree ----------
data HVal 
  = HInteger Integer -- No Support For Floats
  | HBool Bool
  | HNull 
  | HString String
  | HChar Char
  | HList [HVal]
  | HObj [(String, HVal)]
  deriving (Show, Eq, Read)

data Op = Add | Sub | Mult | Div | Mod
        deriving (Show, Read)

--sOp :: String -> Op
--sOp s 
 -- | s == "+" 		= Add
  -- | s == "-" 		= Sub
  -- | s == "*" 		= Mult
  -- | s == "div" 		= Div
  -- | s == "mod"		= Mod
  -- | otherwise 	 	= undefined
  


newtype Parser a = Parser {
 runParser :: String -> Maybe (String, a)
}

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      return (input', f x)

--instance Applicative Parser where
  --pure x = Parser $ \input -> Just (input, x)
  --(Parser p) <*> (Parser q) = Parser $ \input -> do
--	  			(input', f) <- p input
--				(input', a) <- q input
--				Just (input', f a)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p) <*> (Parser q) = Parser $ \input -> do
                (input', f) <- p input
                (input'', a) <- q input'
                Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p) <|> (Parser q) =
	  Parser $ \input -> (p input <|> q input)


parseExpr :: Parser HVal
parseExpr = parseInteger <|> parseString <|> parseList <|> parseBool

charP :: Char -> Parser Char
charP x = Parser $ f
  where f (y:ys)
	   | y == x = Just (ys, x)
           | otherwise = Nothing
	f []  = Nothing


hNull :: Parser HVal
hNull = undefined

stringP :: String -> Parser String
stringP = sequenceA . map charP

parseBool :: Parser HVal
parseBool = f <$> (stringP "true" <|> stringP "false")
  where
    f "true"  = HBool True
    f "false" = HBool False
    f x       = HString x


ops :: [Char]
ops = ['+']

isOp :: Char -> Bool
isOp c = elem c ops

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> let (token, rest) = span f input
                              in Just (rest, token)


notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
			(input', xs) <- p input
			if null xs
     				then Nothing
				else Just (input', xs) 

parseInteger :: Parser HVal
parseInteger = f <$> notNull (spanP isDigit)
  where f ds = HInteger $ read ds 

op :: Parser Op
op = sOp <$> notNull (spanP isOp)

stringLiteral :: Parser String
stringLiteral = spanP (/= '"')  -- Check with filter && no esacpe support

opLiteral :: Parser String
opLiteral = spanP isOp

sOp :: String -> Op
sOp n | n == "*" = Add

parseOp :: Parser Op
parseOp = sOp <$> (charP '"' *> opLiteral <* charP '"')

parseString :: Parser HVal
parseString = HString <$> (charP '"' *> stringLiteral <* charP '"')


whiteSpace :: Parser String
whiteSpace = spanP isSpace

sepBy :: Parser a   -- Parser for the separators
      -> Parser b   -- Parser for elements
      -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

parseList :: Parser HVal
parseList = HList <$> (charP '[' *> whiteSpace *> elements <* whiteSpace <* charP ']')
  where
    elements = sepBy sep parseExpr 
    sep = whiteSpace *> charP ',' <* whiteSpace












main :: IO ()
main = print("All Good") 
