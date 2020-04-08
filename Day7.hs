import DayZero
import AocComputer
import Data.Char
import Data.List



main :: IO ()
main = do
  program <- compile "./day7.txt"
  run program [0, 1, 2, 3, 4]
  run program [5, 6, 7, 8, 9]
  
run :: Program -> [Int] -> IO ()
run program inputs = do
    let inputOrders = permutations inputs
    let values = map (calculateThrusters program) inputOrders
    putStrLn $ show $ maximum $ map last values


calculateThrusters :: Program -> [Int] -> [Int]
calculateThrusters program phases = calculateThrusters' (take 5 $ repeat program) [0] phases


calculateThrusters' :: [Program] -> [Int] -> [Int] -> [Int]
calculateThrusters' (program:programs) inputs (phase:phases) = 
  output ++ (calculateThrusters' (programs ++ [postRun]) output phases)
    where (output, postRun) = runProgram program (phase:inputs)
calculateThrusters' (program:programs) inputs _ =
  if isFinished program then calculateThrusters' programs output []
    else output ++ (calculateThrusters' (programs ++ [postRun]) output [])
      where (output, postRun) = runProgram program inputs
calculateThrusters' [] _ _ = []



