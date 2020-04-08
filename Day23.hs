import AocComputer
import qualified Data.Map as M
import Data.Maybe

type Nat = (Int, Int)

main :: IO ()
main = do
    compiled <- compile "day23.txt"
    let computers = foldl (\m k -> M.insert k compiled m) M.empty [0..49]
    let initiated = M.mapWithKey (\address computer -> runProgram computer [address]) computers
    let firstOutputs = getOutputs initiated
    let runningPrograms = M.map snd initiated

    let results = run runningPrograms firstOutputs
    let results2 = run2 runningPrograms firstOutputs (0, 0) (-1)

    putStrLn $ show $ results
    putStrLn $ show results2


run2 :: M.Map Int Program -> [Int] -> Nat -> Int -> Int
run2 programs [] (natX, natY) prev = 
    let all = M.map (\c -> runProgram c [-1]) programs
        programs' = M.map snd all
        outputs = getOutputs all
    in case outputs of
        [] -> run2 programs' [0, natX, natY] (natX, natY) prev
        _ ->  run2 programs' outputs (natX, natY) prev
run2 programs (a:x:y:outs) nat prev
    | a == 255 = run2 programs outs (x, y) prev
    | prev == a && a == 0 = y
    | otherwise =
        let program = fromJust $ M.lookup a programs
            (output, program') = runProgram program [x, y]
        in run2 (M.insert a program' programs) (output ++ outs) nat a

run :: M.Map Int Program -> [Int] -> Int
run programs [] = 
    let all = M.map (\c -> runProgram c [-1]) programs in
    run (M.map snd all) $ getOutputs all
run programs (a:x:y:outs)
    | a == 255 = y
    | otherwise =
        let program = fromJust $ M.lookup a programs
            (output, program') = runProgram program [x, y]
        in run (M.insert a program' programs) $ output ++ outs


getOutputs :: M.Map Int (Output, Program) -> [Int]
getOutputs programs = concat $ filter ((/=) []) $ map snd $ M.toList $ M.map fst programs