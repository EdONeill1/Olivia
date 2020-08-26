import System.IO
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding ( spaces )
import System.Environment
import Prelude hiding (head, tail)
import Control.Applicative hiding ((<|>), many)
import Text.Parsec hiding ((<|>))
import Data.IORef
instance Show HVal where show = showVal

data HVal = Integer Integer
          | String String
	  | Boolean Bool
	  | List [HVal]
          | Op Op
          | Expr Op HVal HVal
	  | Car [HVal]
          | Cdr [HVal]
	  | Cons [HVal]
 	  | Of [HVal] HVal
  	  | Assign HVal HVal
           deriving (Read)

data Op = Add | Mult | Sub | Div | Mod | And | Or | Greater | GreaterEq | Less | LessEq | Equ | Not deriving (Show, Read)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces' :: Parser()
spaces' = skipMany1 space

---------- PARSERS ----------
parseString :: Parser HVal
parseString = do
		_ <- char '"'
		x <- many (noneOf "\"")
		_ <- char '"'
		return $ String x

-- parseChar can parse both characters and strings.
parseChar :: Parser HVal
parseChar = many1 (letter <|> space) >>= (\x -> return $ case x of
							   "True" -> Boolean True
							   "False" -> Boolean False
							   "T" -> Boolean True
							   "F" -> Boolean False
							   _ -> String $ x)

parseBool :: Parser HVal
parseBool = many1 letter >>= (\x -> return $ case x of
					       "True" -> Boolean True
					       "False" -> Boolean False)
parseInteger :: Parser HVal
parseInteger = many1 digit >>= (\x -> return $ Integer $ read x)

parseList :: Parser HVal
parseList = liftM List $ sepBy parseExpr spaces'

parseOp :: Parser HVal
parseOp = (string "+" <|> string "-" <|> string "*" <|> string "/" <|> string "%" <|> string "&&" <|> string "||" <|> string "<" <|> string "<=" <|> string ">" <|> string ">=" <|> string "==" <|> string "!" ) >>= 
	  (\x -> return $ case x of
		     		"==" -> Op Equ
				"!" -> Op Not
		     		"<" -> Op Less
				"<=" -> Op LessEq
				">" -> Op Greater
				">=" -> Op GreaterEq	
				"&&" -> Op And
 				"||" -> Op Or
				"+" -> Op Add
				"-" -> Op Sub
				"*" -> Op Mult
				"/" -> Op Div
				"%" -> Op Mod)


parseExpression :: Parser HVal
parseExpression = do
 		    x <- parseHead <|> parseOf <|> parseInteger <|> parseBool 
		    op <- parseOp
		    y <- parseHead <|> parseOf <|> parseInteger <|> parseBool <|> do
			   		   		 			    _ <- char '('
					   		 			    z <- parseExpression
					   					    _ <- char ')'
					   		 			    return z 
		    return $ case op of
		 		Op And -> Expr And x y
				Op Or -> Expr Or x y
				Op Add -> Expr Add x y
				Op Sub -> Expr Sub x y
				Op Mult -> Expr Mult x y
				Op Div -> Expr Div x y
				Op Mod -> Expr Mod x y
				Op Less -> Expr Less x y
				Op LessEq -> Expr LessEq x y
				Op Greater -> Expr Greater x y
				Op GreaterEq -> Expr GreaterEq x y
				Op Equ -> Expr Equ x y
				Op Not -> Expr Not x y


--- LIST FUNCTIONS ---
hVal2Int :: HVal -> Integer
hVal2Int (Integer x) = x

head :: [HVal] -> HVal
head [List (x:xs)] = x

tail :: [HVal] -> ThrowsError HVal
tail [List (x:xs)] = return $ List xs

cons :: [HVal] -> ThrowsError HVal
cons [List xs, List []] = return $ List $ xs
cons [List xs, List ys] = return $ List $ xs ++ ys

of' :: [HVal] -> Integer -> HVal
of' [List xs] n = xs!!(fromIntegral n)

-----------------------
	
parseOf :: Parser HVal
parseOf = do
		_ <- string "__"
		_ <- char '['
		x <- parseList
   		_ <- char ']'
		_ <- string "."
		n <- parseInteger
		return $ Of [x] n

parseHead :: Parser HVal
parseHead = do
		_ <- string "head "
		_ <- char '['
		x <- parseList
		_ <- char ']'
		return $ Car [x]

parseTail :: Parser HVal
parseTail = do
		_ <- string "tail "
		_ <- char '['
		x <- parseList
		_ <- char ']'
		return $ Cdr [x]

parseCons :: Parser HVal
parseCons = do
		_ <- string "cons "
		_ <- char '['
		x <- parseList <|> parseExpr
		_ <- char ']'
		_ <- char ' '
		_ <- char '['
		y <- parseList <|> parseExpr
		_ <- char ']'
		return $ Cons [x, y]

parseListFunctions :: Parser HVal
parseListFunctions = parseHead <|> parseTail <|> parseCons <|> parseOf

parseGCLFunctions :: Parser HVal
parseGCLFunctions = parseAssign

parseAtomicFunctions :: Parser HVal
parseAtomicFunctions = parseInteger 
                    <|> parseChar

parseAssign :: Parser HVal
parseAssign = do
		x <- parseChar
		_ <- string ":=" <|> string " := "
		y <- parseInteger 
		return $ Assign x y

parseExpr :: Parser HVal
parseExpr =  parseInteger
	 <|> parseListFunctions
   	 <|> parseAssign
	 <|> parseChar
         <|> do
		_ <- char '['
                x <- parseList
                _ <- char ']'
                return x
         <|> do
	     	_ <- char '('
	     	x <- parseExpression
             	_ <- char ')'
             	return x

---------- ERROR CHECKING AND EXCEPTIONS ----------

data HError = NumArgs Integer [HVal]
	    | TypeMismatch String HVal
   	    | Parser ParseError
      	    | BadSpecialForm String HVal
	    | NotFunction String String
   	    | UnboundVar String String
            | Default String

showError :: HError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ "args: found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: Expected " ++ expected ++ " , Found: " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show HError where show = showError

instance Error HError where
  noMsg = Default "An Error Has Occured"
  strMsg = Default

type ThrowsError = Either HError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


---------------------------------------------------
	
---------- Building a REPL: Basic I/O ----------
flushStr :: String -> IO()
flushStr str = putStr str >> hFlush stdout


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

runRepl :: IO()
runRepl = until_ (== "quit") (readPrompt "H > ") evalAndPrint

------------------------------------------------
showVal :: HVal -> String
showVal (Integer number) = show number
showVal (String string) = "'" ++ string ++ "'"
showVal (Boolean bool) = show bool
showVal (List list) = "[" ++ unwordsList list ++ "]"
showVal (Expr op x y) = show x ++ " " ++ show op ++ " " ++ show y
showVal (Car xs) = show (head xs)
showVal (Cdr xs) = show (tail xs)
showVal (Cons xs) = show xs 
showVal (Of xs n) = show n
showVal (Assign x y) = show x ++ " := " ++ show y
unwordsList :: [HVal] -> String 
unwordsList xs = unwords (map showVal xs)

evalExpr :: HVal -> Op -> HVal -> ThrowsError HVal
evalExpr (Integer x) Add (Integer y) = return $ Integer ((x) + (y))
--evalExpr (Integer x) Add (Expr op z y) = return $ Integer (x + (eval $ evalExpr op z y))
evalExpr (Integer x) Sub (Integer y) = return $ Integer (x - y)
evalExpr (Integer x) Mult (Integer y) = return $ Integer (x * y)
evalExpr (Integer x) Div (Integer y) = return $ Integer (x `div` y)
evalExpr (Integer x) Mod (Integer y) = return $ Integer (x `mod` y)
evalExpr (Integer x) Less (Integer y) = return $ Boolean (x < y)
evalExpr (Integer x) LessEq (Integer y) = return $ Boolean (x <= y)
evalExpr (Integer x) Greater (Integer y) = return $ Boolean (x > y)
evalExpr (Integer x) GreaterEq (Integer y) = return $ Boolean (x >= y)
evalExpr (Boolean x) And (Boolean y) = return $ Boolean (x && y)
evalExpr (Boolean x) Or (Boolean y) = return $ Boolean (x || y)
evalExpr (Integer x) Equ (Integer y) = return $ Boolean (x == y)
evalExpr (Boolean x) Equ (Boolean y) = return $ Boolean (x == y)

eval' :: HVal -> Op -> HVal -> HVal
eval' (Integer x) Add (Integer y) = Integer (x+y)
eval' (Integer x) Mult (Integer y) = Integer (x*y)
eval' (Integer x) Sub (Integer y) = Integer (x-y)
eval' (Integer x) Div (Integer y) = Integer (x `div` y)
eval' (Integer x) Mod (Integer y) = Integer (x `mod` y)
eval' (Integer x) Less (Integer y) = Boolean (x<y)
eval' (Integer x) LessEq (Integer y) = Boolean (x<=y)
eval' (Integer x) Greater (Integer y) = Boolean (x>y)
eval' (Integer x) GreaterEq (Integer y) = Boolean (x>=y)
eval' (Boolean x) And (Boolean y) = Boolean (x&&y)
eval' (Boolean x) Or (Boolean y) = Boolean (x || y)
eval' (Integer x) Add (Expr op a b) = eval' (Integer x) Add (eval' a op b)
eval' (Integer x) Mult  (Expr op a b) = eval' (Integer x) Mult (eval' a op b)
eval' (Integer x) Sub (Expr op a b) = eval' (Integer x) Sub (eval' a op b)
eval' (Integer x) Div (Expr op a b) = eval' (Integer x) Div (eval' a op b)
eval' (Integer x) Mod (Expr op a b) = eval' (Integer x) Mod (eval' a op b)
eval' (Integer x) Less (Expr op a b) = eval' (Integer x) Less (eval' a op b)
eval' (Integer x) LessEq (Expr op a b) = eval' (Integer x) LessEq (eval' a op b)
eval' (Integer x) Greater (Expr op a b) = eval' (Integer x) Greater (eval' a op b)
eval' (Integer x) GreaterEq (Expr op a b) = eval' (Integer x) GreaterEq (eval' a op b)
eval' (Boolean x) And (Expr op a b) = eval' (Boolean x) And (eval' a op b)
eval' (Boolean x) Or (Expr op a b) = eval' (Boolean x) Or (eval' a op b)
eval' (Integer x) Add (Car ys) =  eval' (Integer x) Add (head ys)
eval' (Car xs) Add (Integer y) = eval' (head xs) Add (Integer y)
eval' (Integer x) Mult (Car ys) =  eval' (Integer x) Mult (head ys)
eval' (Car xs) Mult (Integer y) = eval' (head xs) Mult (Integer y)
eval' (Integer x) Sub (Car ys) =  eval' (Integer x) Sub (head ys)
eval' (Car xs) Sub (Integer y) = eval' (head xs) Sub (Integer y)
eval' (Integer x) Div (Car ys) =  eval' (Integer x) Div (head ys)
eval' (Car xs) Div (Integer y) = eval' (head xs) Div (Integer y)
eval' (Integer x) Mod (Car ys) =  eval' (Integer x) Mod (head ys)
eval' (Car xs) Mod (Integer y) = eval' (head xs) Mod (Integer y)
eval' (Integer x) Less (Car ys) =  eval' (Integer x) Less (head ys)
eval' (Car xs) Less (Integer y) = eval' (head xs) Less (Integer y)
eval' (Integer x) LessEq (Car ys) =  eval' (Integer x) LessEq (head ys)
eval' (Car xs) LessEq (Integer y) = eval' (head xs) LessEq (Integer y)
eval' (Integer x) Greater (Car ys) =  eval' (Integer x) Greater (head ys)
eval' (Car xs) Greater (Integer y) = eval' (head xs) Greater (Integer y)
eval' (Integer x) GreaterEq (Car ys) =  eval' (Integer x) GreaterEq (head ys)
eval' (Car xs) GreaterEq (Integer y) = eval' (head xs) GreaterEq (Integer y)
eval' (Integer x) And (Car ys) =  eval' (Integer x) And (head ys)
eval' (Car xs) And (Integer y) = eval' (head xs) And (Integer y)
eval' (Integer x) Or (Car ys) =  eval' (Integer x) Or (head ys)
eval' (Car xs) Or (Integer y) = eval' (head xs) Or (Integer y)
eval' (Integer x) Add (Of ys (Integer n)) = eval' (Integer x) Add (of' ys n)
eval' (Of xs (Integer n)) Add (Integer y) = eval' (of' xs n) Add (Integer y)
eval' (Integer x) Sub (Of ys (Integer n)) = eval' (Integer x) Sub (of' ys n)
eval' (Of xs (Integer n)) Sub (Integer y) = eval' (of' xs n) Sub (Integer y)
eval' (Integer x) Mult (Of ys (Integer n)) = eval' (Integer x) Mult (of' ys n)
eval' (Of xs (Integer n)) Mult (Integer y) = eval' (of' xs n) Mult (Integer y)
eval' (Integer x) Div (Of ys (Integer n)) = eval' (Integer x) Div (of' ys n)
eval' (Of xs (Integer n)) Div (Integer y) = eval' (of' xs n) Div (Integer y)
eval' (Integer x) Mod (Of ys (Integer n)) = eval' (Integer x) Mod (of' ys n)
eval' (Of xs (Integer n)) Mod (Integer y) = eval' (of' xs n) Mod (Integer y)
eval' (Integer x) Less (Of ys (Integer n)) = eval' (Integer x) Less (of' ys n)
eval' (Of xs (Integer n)) Less (Integer y) = eval' (of' xs n) Less (Integer y)
eval' (Integer x) LessEq (Of ys (Integer n)) = eval' (Integer x) LessEq (of' ys n)
eval' (Of xs (Integer n)) LessEq (Integer y) = eval' (of' xs n) LessEq (Integer y)
eval' (Integer x) Greater (Of ys (Integer n)) = eval' (Integer x) Greater (of' ys n)
eval' (Of xs (Integer n)) Greater (Integer y) = eval' (of' xs n) Greater (Integer y)
eval' (Integer x) GreaterEq (Of ys (Integer n)) = eval' (Integer x) GreaterEq (of' ys n)
eval' (Of xs (Integer n)) GreaterEq (Integer y) = eval' (of' xs n) GreaterEq (Integer y)
eval' (Integer x) And (Of ys (Integer n)) = eval' (Integer x) And (of' ys n)
eval' (Of xs (Integer n)) And (Integer y) = eval' (of' xs n) And (Integer y)
eval' (Integer x) Or (Of ys (Integer n)) = eval' (Integer x) Or (of' ys n)
eval' (Of xs (Integer n)) Or (Integer y) = eval' (of' xs n) Or (Integer y)




eval :: HVal -> ThrowsError HVal
eval val@(Integer _) = return val
eval val@(String _) = return val
eval val@(Boolean _) = return val
eval val@(List _) = return val -- Look at list eval NOT WORKING
eval val@(Op _) = return val
eval (Expr op x y) = eval $ eval' (x) op (y)
eval (Car xs) = eval $ head xs
eval (Cdr xs) = tail xs
eval (Cons xs) = cons xs
eval (Of [xs] n) = eval $ of' [xs] (hVal2Int n)

readExpr :: String -> ThrowsError HVal
readExpr input = case parse parseExpr "H" input of
		    Left err -> throwError $ Parser err
		    Right val -> return val


main :: IO()
main = do
	args <- getArgs
	case length args of
   		0 -> runRepl
		1 -> evalAndPrint $ args !! 0
		otherwise -> putStrLn "Enter 0 or 1 arguments King"

--main = getArgs >>= putStrLn . show . eval . readExpr . (!!0)
--main = do args <- getArgs
--	  putStrLn (readExpr (args!! 0))
