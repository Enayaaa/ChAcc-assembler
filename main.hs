import System.Environment
import Debug.Trace


main = do
    args <- getArgs
    mapM_ putStrLn args
