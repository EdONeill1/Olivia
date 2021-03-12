import Data.Foldable (traverse_)
import Control.Concurrent.Async
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative

data If = If (Bool, Int) deriving (Show, Eq, Read)


factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n - 1)


ifs = [If (False, 5), If (True, 1), If (True, 4), If (False, 6)]

filterTrueBranches (If (g,s) : gs)
  | gs == [] = if g == True then [s] else []
  | g == False = filterTrueBranches (head gs : tail gs)
  | otherwise = s : [] ++ filterTrueBranches (head gs : tail gs)

executeTrueBranches branches = map (\s -> factorial s) branches



--main = executeTrueBranches $ filterTrueBranches ifs

a1 :: IO Int
a1 = do
        threadDelay 100000
        return 10


a2 :: IO Int
a2 = do
        return 5

-- raceAll will faciliate non-determinism as it will produce the fastest result of n threads.
raceAll :: [IO a] -> IO a
raceAll [] = fail "whoops"
raceAll acts = go acts where
    go [io] = io
    go ios = go (raceAdjacent ios)
    raceAdjacent (io:io':ios) = (either id id <$> race io io') : raceAdjacent ios
    raceAdjacent short = short

-- Obtaining Integer values from Either type produced by race function.
r x y = race x y >>= \z -> case z of
                             Left l -> print l
                             Right r -> print r



calculateFactorial :: Int -> IO Int
calculateFactorial n = do
        threadDelay 500000
        return $ factorial n
        

main :: IO ()
main = do
        res <- async <$> (filterTrueBranches ifs) $ \a1 -> atomically $ waitSTM a1 
        putStrLn $ show res
        return ()

