import DayZero
import Data.Char
import AocComputer

main :: IO ()
main = do
  program <- compile "./day5.txt"
  putStrLn $ show $ head $ fst $ runProgram program [1]
  putStrLn $ show $ head $ fst $ runProgram program [5]