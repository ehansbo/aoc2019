import AocComputer
import Data.Char
import Data.List



main :: IO ()
main = do
    program <- compile "./day9.txt"
    run program
  
run :: Program -> IO ()
run program = do
  let (out1, _) = runProgram program [1]
  let (out2, _) = runProgram program [2]
  putStrLn $ show out1
  putStrLn $ show out2

