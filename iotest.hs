
f :: IO String
f = do x <- getContents
       return x