import Control.Concurrent
import Control.Concurrent.Async

action1 :: IO Int
action1 = do
    threadDelay 500000 -- just to make it interesting
    return 5

action2 :: IO String
action2 = do
    threadDelay 1000000
    return "action2 result"

main :: IO ()
main = do
    res <- concurrently action1 action2
    print (res :: (Int, String))
