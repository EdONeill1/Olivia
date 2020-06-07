-- _______________

-- HENRY PROGRAMS
-- _______________
-- k0, k1, ... , k.n := a,b, ... , z
-- ;Do n != N ->
--     k0, k1, ... , k.n := a,b, ... , z
--  Od


-- k0, k1, ... , k.n := a,b, ... , z
-- ;Do n != N ->
--     if P(x) -> f . x
--     [] !P(x) -> !f . x
--     fi
--  Od
import System.Environment
import System.IO
import Prelude
import Data.IORef
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Char
instance Show HenryVal where show = showVal
type Env = IORef [(String, IORef HenryVal)] 

--- Definition of data, language, and lexer
data HenryVal = Atom String
              | String String 
              | Integer Integer
              | Bool Bool
              | Not HenryVal
              | Neg HenryVal
              | List [HenryVal]
              | Seq [HenryVal]
              | Assign String HenryVal
              | If HenryVal HenryVal HenryVal
              | While HenryVal HenryVal
              | Skip
              | ABinOp ABinOp
              | RBinOp RBinOp
              | ABinary ABinOp HenryVal HenryVal
              | BBinary BBinOp HenryVal HenryVal
              | RBinary RBinOp HenryVal HenryVal
               deriving (Read)

data BBinOp = And | Or deriving (Show, Read)

data RBinOp = Greater
            | GEqual
            | Less
            | LEqual
             deriving (Show, Read)

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
            | Modulo
              deriving (Show, Read)

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
                                     , "["
                                     , "]"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                      , "<", ">", "and", "or", "not"
                                     ]
           }
lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

----------

--- Definition of Parsers
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser HenryVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser HenryVal
parseAtom = do 
              first <- letter
              rest <- many (letter <|> digit)
              let atom = first:rest
              return $ case atom of 
                         "True" -> Bool True
                         "False" -> Bool False
                         "Add" -> ABinOp Add
                         "Subtract" -> ABinOp Subtract
                         "Multiply" -> ABinOp Multiply
                         "Divide" -> ABinOp Divide
                         "Modulo" -> ABinOp Modulo
                         "Greater" -> RBinOp Greater
                         "Less" -> RBinOp Less
                         "GEqual" -> RBinOp GEqual
                         "LEqual" -> RBinOp LEqual
                         _    -> Atom atom

parseBinary :: Parser HenryVal
parseBinary = do
                x <- parseNumber <|> parseString <|> parseAtom
                op <- oneOf "/*+-<>gl^|" 
                y <- parseNumber <|> parseString <|> parseAtom
                if op == '*' 
                    then 
                        return $ ABinary Multiply x y 
                else
                    if op == '/' then
                        return $ ABinary Divide x y 
                    else 
                        if op == '+' then
                            return $ ABinary Add x y
                        else 
                            if op == '-' then 
                                return $ ABinary Subtract x y
                            else
                                if op == '<' then
                                    return $ RBinary Less x y
                                else
                                    if op == '>' then
                                        return $ RBinary Greater x y
                                    else
                                        if op == 'l' then
                                            return $ RBinary LEqual x y
                                        else
                                            if op == 'g' then
                                                return $ RBinary GEqual x y
                                            else
                                                if op == '^' then
                                                    return $ BBinary And x y
                                                else
                                                    if op == '|' then
                                                        return $ BBinary Or x y
                                                    else
                                                        return $ String "Error"


parseNumber :: Parser HenryVal
parseNumber = liftM (Integer . read) $ many1 digit

parseList :: Parser HenryVal
parseList = liftM List $ sepBy parseExpr spaces

statement :: Parser HenryVal
statement =   parens statement
          <|> sequenceOfStmt

sequenceOfStmt =
  do list <- (sepBy1 statement' semi)
     return $ if length list == 1 then head list else Seq list


henryParser :: Parser HenryVal
henryParser = whiteSpace >> statement


statement' :: Parser HenryVal
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> assignStmt
  

parseExpr :: Parser HenryVal   
parseExpr =   henryParser
          <|> parseAtom
          <|> parseNumber 
          <|> parseString
          <|> do 
                _ <- char '['
                x <- try parseList
                _ <- char ']'
                return x
          <|> do 
                _ <- char '<'
                x <- try parseBinary
                _ <- char '>'
                return x


ifStmt :: Parser HenryVal
ifStmt =
  do reserved "if"
     cond  <- bExpression <|>
              do 
                _ <- char '<'
                x <- try parseBinary
                _ <- char '>'
                return x
     reserved " []"
     stmt1 <- statement
     reserved " []"
     stmt2 <- statement
     return $ If cond stmt1 stmt2
  <|>
  do reserved "if"
     cond  <- rExpression
     reserved " []"
     stmt1 <- statement
     reserved " []"
     stmt2 <- statement
     return $ If cond stmt1 stmt2


whileStmt :: Parser HenryVal
whileStmt =
  do reserved ";Do"
     cond  <- bExpression <|>
              do 
                _ <- char '<'
                x <- try parseBinary
                _ <- char '>'
                return x
     reserved "->"
     stmt <- statement <|>
             do 
                _ <- char '<'
                x <- try parseBinary
                _ <- char '>'
                return x
     reserved " Od"
     return $ While cond stmt

assignStmt :: Parser HenryVal
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- do 
                _ <- char '<'
                x <- try parseBinary
                _ <- char '>'
                return x
            <|>
                parseNumber
            <|>
                parseString
     return $ Assign var expr

skipStmt :: Parser HenryVal
skipStmt = reserved "skip" >> return Skip

aExpression :: Parser HenryVal
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser HenryVal
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
            
aTerm =  parens aExpression
     <|> liftM String identifier
     <|> liftM Integer integer

   
bTerm =  parens bExpression <|> (reserved "true"  >> return (Bool True )) <|> (reserved "false" >> return (Bool False)) <|> rExpression

rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ RBinary op a1 a2

relation = (reservedOp ">" >> return Greater) <|> (reservedOp "<" >> return Less)


parseFile :: String -> IO HenryVal
parseFile file =
  do program  <- readFile file
     case parse henryParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r


--- Beginning of evaluation
showVal :: HenryVal -> String
showVal (Atom contents) = show contents
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Integer contents) = show contents
showVal (Neg contents) = "-" ++ show contents
showVal (Bool True) = "True"
showVal (Bool False) = "False"
showVal (Not contents) = "Not " ++ show contents
showVal (List contents) = "[" ++ unwordsList contents ++ "]"
showVal (Seq contents) = unwordsList contents
showVal (Assign var val) = var ++ " := " ++ show val
showVal (If cond stmt1 stmt2) = "if " ++ show cond ++ " then " ++ show stmt1 ++ " else " ++ show stmt2 
showVal (While cond stmt) = ";Do " ++ show cond ++ " -> " ++ show stmt ++ " Od"
showVal (Skip) = "Skip"
showVal (ABinary op x y) = show x ++ " " ++ show op ++ " " ++ show y 
showVal (BBinary op x y) = show x ++ " " ++ show op ++ " " ++ show y 
showVal (RBinary op x y) = show x ++ " " ++ show op ++ " " ++ show y 
showVal (ABinOp op) = show op
showVal (RBinOp op) = show op

unwordsList :: [HenryVal] -> String
unwordsList = unwords . map showVal

evalABinOp :: Env -> HenryVal -> ABinOp -> HenryVal -> IOThrowsError HenryVal
evalABinOp env (Integer a) Add (Integer b)   = return $ Integer $ a + b
evalABinOp env (Integer a) Subtract (Integer b)   = return $ Integer $ a - b
evalABinOp env (Integer a) Multiply (Integer b)   = return $ Integer $ a * b
evalABinOp env (Integer a) Divide (Integer b)   = return $ Integer $ a `div` b
evalABinOp env (Atom a)    op  b@(Integer _) = getVar env a >>= (\c -> evalABinOp env c op b)
evalABinOp env a@(Integer _)    op  (Atom b) = getVar env b >>= (\c -> evalABinOp env a op c)
evalABinOp env (Atom a)    op (Atom b) = getVar env a >>= (\c -> evalABinOp env c op c)

evalBBinOp :: Env -> HenryVal -> BBinOp -> HenryVal -> IOThrowsError HenryVal
evalBBinOp env (Bool a) And (Bool b) = return $ Bool (a && b)
evalBBinOp env (Bool a) And (Not (Bool b)) = return $ Bool (a && b)
evalBBinOp env (Not (Bool a)) And (Bool b) = return $ Bool (a && b)
evalBBinOp env (Not (Bool a)) And (Not (Bool b)) = return $ Bool (a && b)
evalBBinOp env (Bool a) Or (Bool b) = return $ Bool (a || b)
evalBBinOp env (Bool a) Or (Not (Bool b)) = return $ Bool (a || b)
evalBBinOp env (Not (Bool a)) Or (Bool b) = return $ Bool (a || b)
evalBBinOp env (Not (Bool a)) Or (Not (Bool b)) = return $ Bool (a || b)

evalRBinOp :: Env -> HenryVal -> RBinOp -> HenryVal -> IOThrowsError HenryVal
evalRBinOp env (Integer a) Greater (Integer b) = return $ Bool (a > b)
evalRBinOp env (Integer a) Less (Integer b) = return $ Bool (a < b)
evalRBinOp env (Integer a) GEqual (Integer b) = return $ Bool (a >= b)
evalRBinOp env (Integer a) LEqual (Integer b) = return $ Bool (a <= b)
evalRBinOp env (Atom a)    op  b@(Integer _) = getVar env a >>= (\c -> evalRBinOp env c op b)

-- evalCond :: HenryVal -> Bool
-- evalCond (Bool cond) = if cond then True else False
-- evalCond (String cond) = if  (henryBool2Bool (eval (str2rbinary cond))) then True else False
--evalCond (RBinary cond) = if (evalRBinOp (RBinary cond)) then True else False
--evalCond (String cond) = if (henryBool2Bool (evalRBinOp (int2HenryInt (str2Int ((words cond) !! 0))) (evalROp ((words cond) !! 1)) (int2HenryInt (str2Int ((words cond) !! 2))) )) == True then True else False
--evalCond (List cond) = if (henryBool2Bool (evalRBinOp (cond !! 0) ((henryVal2Rop (cond !! 1))) (cond !! 2))) == True then True else False

-- evalWhile :: HenryVal -> HenryVal -> HenryVal
-- evalWhile cond stmt = if (henryBool2Bool cond) == False then (Bool False) else eval stmt

-- evalCond :: HenryVal -> Bool
-- evalCond (Bool cond) = if cond then return $ (Bool True) else return $ (Bool False)
-- evalCond env (String cond) = if (return $ eval env cond) then return $ (Bool True) else return $ (Bool False)

-- evalCond :: Env -> HenryVal -> HenryVal
-- evalCond env (Bool cond) = if cond then Bool True else Bool False   
-- evalCond env (String cond) = if (henryBool2Bool (return $ eval env (str2rbinary cond))) then Bool True else Bool False 


henryVal2Rop :: HenryVal -> RBinOp
henryVal2Rop (RBinOp Less) = Less
henryVal2Rop (RBinOp Greater) = Greater

str2Int :: String -> Integer
str2Int str = read (str) :: Integer

-- str2rbinary :: String -> HenryVal
-- str2rbinary string = RBinary (evalROp ((words string) !! 1)) (int2HenryInt (str2Int ((words string) !! 0))) (int2HenryInt (str2Int ((words string) !! 2)))

int2HenryInt :: Integer -> HenryVal
int2HenryInt num = Integer num

henryBool2Bool :: HenryVal -> Bool
henryBool2Bool (Bool True) = True
henryBool2Bool (Bool False) = False
henryBool2Bool (String "True") = True
henryBool2Bool (String "False") = False


evalWhile :: Env -> HenryVal -> HenryVal -> IOThrowsError HenryVal
evalWhile env cond stmt = eval env stmt >>= (\c -> do
                                                     s <- eval env cond
                                                     if (henryBool2Bool s) == False
                                                         then return $ c
                                                            else eval env (While cond stmt))



-- evalWhile :: Env -> HenryVal -> HenryVal -> IOThrowsError HenryVal
-- evalWhile env cond stmt = do
--                             x <- eval env cond
--                             whileM (henryBool2Bool x) (eval env stmt)
-- exec (While e s)    = do v <- eval e
--                          when (v /= 0) (exec (Seq [s, While e s]))
                      
-- hello_worlds :: Int -> Bool -> IO ()
-- hello_worlds n bool 
--   | bool == False = replicateM_ 1 $ putStrLn "Hello World"
--   | otherwise = do
--                     putStrLn "Hello"
--                     hello_worlds n False

evalROp :: Env -> String -> IOThrowsError HenryVal
evalROp env "Less" = return $ RBinOp Less
evalROp env "Greater" = return $ RBinOp Greater
evalROp env "GEqual" = return $ RBinOp GEqual
evalROp env "LEqual" = return $ RBinOp LEqual

eval :: Env -> HenryVal -> IOThrowsError HenryVal
eval env val@(Atom _) = return val
eval env val@(String _) = return val
eval env val@(Integer _) = return val
eval env val@(Bool _) = return val
eval env val@(Neg _) = return val
eval env val@(Not _) = return val
eval env (List [Atom "quote", val]) = return val
eval env (Assign var val) = eval env val >>= defineVar env var
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (Seq [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var
eval env val@(List _) = return val
eval env val@(Seq _) = return val
eval env (If cond x y) = eval env cond >>= (\c -> if (henryBool2Bool c) then (eval env x) else (eval env y))  
--eval env (While cond stmt) = eval env cond >>= (\c -> if (henryBool2Bool c) == False then (eval env stmt) else eval env (While cond stmt)        )
eval env (While cond stmt) = evalWhile env cond stmt
-- -- eval env (While cond stmt) = eval env cond >>= (\c -> if (henryBool2Bool c) == False then (eval env stmt) else do 
-- --                                                                                                                  x <- eval env stmt
-- --                                                                                                                  eval env $ While c x)                                                                                                    
eval env val@(ABinOp _) = return val
eval env val@(RBinOp _) = return val
eval env (ABinary op x y) = evalABinOp env x op y
eval env (BBinary op x y) = evalBBinOp env x op y
eval env (RBinary op x y) = evalRBinOp env x op y




readExpr :: String -> ThrowsError HenryVal
readExpr input = case parse parseExpr "Henry" input of
    Left err -> throwError $ Parser err
    Right val -> return val


str2HenryStr :: String -> HenryVal
str2HenryStr s = String $ s

henryStr2Str :: HenryVal -> String
henryStr2Str s = show s

extractInt :: HenryVal -> Integer
extractInt (Integer n) = n
extractInt (String n) = read n :: Integer

extractString :: HenryVal -> String
extractString (String n) = n
extractString (Atom n) = n

------------------------------
          -- REPL --
------------------------------

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Henry > ") . evalAndPrint

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> runOne $ args !! 0
               otherwise -> putStrLn "Program takes only 0 or 1 argument"


------------------------------------------------------------
                -- Variable Assignment --
------------------------------------------------------------

-- Left to do:
--     * Figure out assignment 
--     * Figure out while statements
--     * Figure out arithmetic in regards to variables op integer rather than just integers
--     * Loading .hy files into the console

str2Int' :: HenryVal -> HenryVal
str2Int' n = Integer ( toInteger ( (ord (show n !! 1) - 48)) )

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError HenryVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> HenryVal -> IOThrowsError HenryVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> HenryVal -> IOThrowsError HenryVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: Env -> [(String, HenryVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

--------------------------------------------------
                -- Error Handling --
--------------------------------------------------

data HenryError = NumArgs Integer [HenryVal]
               | TypeMismatch String HenryVal
               | Parser ParseError
               | BadSpecialForm String HenryVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: HenryError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show HenryError where show = showError

type ThrowsError = Either HenryError
type IOThrowsError = ExceptT HenryError IO

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val