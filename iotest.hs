

getLineFoo :: IO String
getLineFoo = do
                x <- getLine
                y <- getLine
                z <- getLine
                return (x++y++z)



getDepth :: Char -> Int
getDepth 'i' = 1
getDepth ';' = 2
getDepth '<' = 0
getDepth '>' = 0
getDepth '[' = 0
getDepth ']' = 0
getDepth 'O' = 0
getDepth _ = 0


f :: Int -> String -> IO String
f n s
  | n == 0 = do
                x <- getLine
                return $ (s ++ x)
  | otherwise = do
                    x <- getLine
                    let m = n + (getDepth (head x))
                    f (m - 1) (s ++ x)

main :: IO()
main = do
       x <- getLine
       y <- getLine
       z <- getLine
       print(x++y++z)