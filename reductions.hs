
f :: Int -> Int
f n
  | n == 0 = n
  | otherwise = (\x -> (\y -> x + y)) n f (n - 1)


main :: IO ()
main = print (f 10)