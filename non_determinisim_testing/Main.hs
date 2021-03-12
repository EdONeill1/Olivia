import Control.Concurrent.Async
import Data.Foldable (traverse_)

type Guard = Bool
type Statement = Int

data If = If Guard Int deriving (Show)

ifs :: [If]
ifs = [If True 1, If False 0, If True 2]

-- | This function returns a unit value that we don't care about. Using
-- concurrently on two such actions would give us ((), ()).

executeStatement :: If -> IO ()
executeStatement (If guard statement) = do
        putStrLn $ show statement
        return ()

-- | Let's write lots of people to their respective files in parallel, instead
-- of sequentially.
writePeople :: [If] -> IO ()
writePeople = runConcurrently . traverse_ (Concurrently . executeStatement)

-- Note: traverse_ is just mapM_ for Applicative instead instead of Monad.
-- Remember, Concurrently is _not_ a Monad instance.

main :: IO ()
main = writePeople ifs
