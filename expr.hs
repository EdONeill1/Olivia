
import System.IO
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.Environment
instance Show Expr where show = showVal

data Expr = ENumber Int
          | EAdd Expr Expr
          | EIfZeroThenElse Expr Expr Expr
           

parseNumber :: Parser Expr
parseNumber = liftM (ENumber . read) $ many1 digit

parseAddition :: Parser Expr
parseAddition = do
                  x <- ENumber 5
                  _ <- char '+'
                  y <- ENumber 5
                  return $ EAdd x y 

parseExpr :: Parser Expr
parseExpr = parseAddition <|> parseNumber

showVal :: Expr -> String
showVal (ENumber contents) = show contents

-- Here I'm just using "Int" as my runtime values
eval :: Expr -> Int
eval (ENumber x) = x
eval (x `EAdd` y) = (eval x) + (eval y)
eval (EIfZeroThenElse cond thenBranch elseBranch) = if 0 == eval cond then eval thenBranch else eval elseBranch

readExpr :: String -> Expr
readExpr input = case parse parseExpr "Henry" input of
    Left error -> ENumber $ 0
    Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

