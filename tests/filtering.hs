#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Control.Concurrent.Async
import Data.Foldable (traverse_)

type Score = Int
data Person = Person Bool Int

people :: [Person]
people =
    [ Person True 50
    , Person False 60
    ]

-- | This function returns a unit value that we don't care about. Using
-- concurrently on two such actions would give us ((), ()).
writePerson :: Person -> IO ()
writePerson (Person fp score) = writeFile fp (show score)

-- | Let's write lots of people to their respective files in parallel, instead
-- of sequentially.
writePeople :: [Person] -> IO ()
writePeople = runConcurrently . traverse_ (Concurrently . f)

-- Note: traverse_ is just mapM_ for Applicative instead instead of Monad.
-- Remember, Concurrently is _not_ a Monad instance.

main :: IO ()
main = writePeople people

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n - 1)

gs = [(True, 1), (False,10), (True,5)]


f :: [(Bool, Int)] -> [(Int)]
f ((g,s) : gs)
  | gs == [] = if g == False then [] else [s]  
  | g == False = f $ (head gs : tail gs)
  | otherwise = s : [] ++ f (head gs : tail gs)

