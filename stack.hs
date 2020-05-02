
import Data.Char
s :: String
s = "NH2CH2CO11"

toTuple :: String -> [(Char, Int)]
toTuple xs
  | xs == [] = []
  | (ord (head (tail xs))) >= 48 && (ord (head (tail xs))) <= 57 = (head xs, (ord (head (tail xs)))) : toTuple (tail (tail xs))
  | otherwise = (head xs, 1) : toTuple (tail (tail xs))