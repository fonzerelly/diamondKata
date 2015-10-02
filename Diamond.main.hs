import Diamond
import System.Environment

main = do
   args <- getArgs
   putStrLn $ createDiamond $ args !! 0 !! 0
