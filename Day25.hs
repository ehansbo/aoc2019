import AocComputer
import Data.Char

main :: IO ()
main = do
    program <- compile "day25.txt"
    run program []

run :: Program -> [Int] -> IO ()
run program input = do
    let (output, program') = runProgram program input
    putStrLn $ fromAscii output
    newInput <- getLine
    let input' = toAscii $ newInput ++ "\n"
    if isFinished program' then run program input' else run program' input'



toAscii :: String -> [Int]
toAscii = map ord

fromAscii :: [Int] -> String
fromAscii = map chr