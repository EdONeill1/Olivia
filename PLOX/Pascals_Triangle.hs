
factorial :: Int -> Int
factorial n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = n * factorial (n - 1)

equation :: Int -> Int -> Int
equation n r = (factorial n) `div` ( (factorial r) * (factorial (n - r)) )

line :: Int -> Int -> [Int]
line n r
  | n == r = [equation n n]
  | otherwise = [equation n r] ++ line n (r + 1)

print' :: [Int] -> String 
print' xs = (unwords (map show xs) ++ "\n")

triangle n = unwords (map print' (map (\x -> line x 0) [0..n]))


main = do
        x <- getLine
        triangle (read x :: Integer)
