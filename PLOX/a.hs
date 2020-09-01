pascalTriangle = 
    [1] : map nextRow pascalTriangle
  where 
    nextRow = ([1] ++). (++ [1]). pairSum
    pairSum x = zipWith (+) x (tail x)

showPascalTriangle = 
    map listToString pascalTriangle
  where
    listToString = unwords. map show
    

main = getLine >>= putStrLn. unlines. flip take showPascalTriangle. read
