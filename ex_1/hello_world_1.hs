module Main where
import System.Environment

main :: IO ()
main = do
     args <- getArgs
     putStr $ (args !! 0) ++ " + " ++ (args !! 1) ++ " = "
     print $ (read (args !! 0) :: Int) + (read (args !! 1) :: Int)

