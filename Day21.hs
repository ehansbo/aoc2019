import AocComputer
import Data.Char
import Data.List

main :: IO () 
main = do
    program <- compile "day21.txt"
    let (response1, _) = runProgram program getInput1
    --putStrLn $ show $ last response1
    let (response2, _) = runProgram program getInput2
    putStrLn $ show $ last response2
    return ()

getInput1 :: [Int]
getInput1 = toAscii $ intercalate "\n" instructions1 ++ "\nWALK\n"

getInput2 :: [Int]
getInput2 = toAscii $ intercalate "\n" instructions2 ++ "\nRUN\n"

instructions1 :: [String]
instructions1 = ["NOT A T", "NOT B J", "OR T J", "NOT C T", "OR T J", "NOT D T", "NOT T T", "AND T J"]

instructions2 :: [String]
instructions2 = ["OR A J", "AND B J", "AND C J", "NOT J J", "AND D J", "OR E T", "OR H T", "AND T J"]


toAscii :: String -> [Int]
toAscii = map ord

fromAscii :: [Int] -> String
fromAscii = map chr