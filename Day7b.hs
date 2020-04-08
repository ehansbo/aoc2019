import DayZero
import Data.Char
import Data.List

type Val = Int
type Index = Int
type Comp = [Int]
type Output = [Int]

data Instruction = 
  Multiplication Val Val Index
  | Addition Val Val Index
  | Exit
  | Input Index
  | Output Index
  | JumpIfTrue Val Index
  | JumpIfFalse Val Index
  | LessThan Val Val Index
  | Equals Val Val Index deriving (Show)

data State = Unfinished Comp Index Output | Finished Output deriving (Show)


main :: IO ()
main = do
    elements <- fileIntegers "./day7.txt" ","
    run elements
  
run :: Comp -> IO ()
run elements = do
    let inputOrders = permutations [5,6,7,8,9]
    let values = map (calculateThrusters elements) inputOrders
    putStrLn $ show $ maximum (map last values)

calculateThrusters :: Comp -> [Int] -> [Int]
calculateThrusters xs phases = calculateThrusters' (take 5 $ repeat (Unfinished xs 0 [])) [0] phases


calculateThrusters' :: [State] -> [Int] -> [Int] -> [Int]
calculateThrusters' ((Unfinished xs i _):states) inputs (phase:phases) =
  calculateThrusters' (states ++ [postRun]) (getOutputs postRun) phases
    where postRun = calculateList (Unfinished xs i []) (phase:inputs)
calculateThrusters' ((Unfinished xs i _):states) inputs _ =
  calculateThrusters' (states ++ [postRun]) (getOutputs postRun) []
    where postRun = calculateList (Unfinished xs i []) inputs
calculateThrusters' ((Finished outputs):states) _  _ = outputs ++ calculateThrusters' states outputs []
calculateThrusters' [] _ _ = []

getOutputs (Unfinished _ _ out) = out
getOutputs (Finished out) = out


calculateList :: State -> [Int] -> State
calculateList (Unfinished xs i outputs) inputs = do 
    let instruction = getInstruction xs i
        (i', xs', inputs') = changeList inputs instruction xs i 
        continue = calculateList (Unfinished xs' i' outputs) inputs' in
      case instruction of
        Exit -> Finished outputs
        Output index -> calculateList (Unfinished xs (i + 2) (outputs ++ [(xs !! index)])) inputs
        Input index -> case inputs of
          [] -> Unfinished xs i outputs
          _ -> continue
        _ -> continue


changeList :: [Int] -> Instruction -> Comp -> Int -> (Int, [Int], [Int])
changeList inputs (Multiplication x y index) xs i = (i + 4, l1 ++ [(x * y)] ++ l2, inputs)
    where (l1, _:l2) = splitAt index xs
changeList inputs (Addition x y index) xs i = (i + 4, l1 ++ [(x + y)] ++ l2, inputs)
    where (l1, _:l2) = splitAt index xs
changeList (input:inputs) (Input index) xs i = (i + 2, l1 ++ [input] ++ l2, inputs)
    where (l1, _:l2) = splitAt index xs
changeList inputs (JumpIfTrue bool index) xs i  = if bool /= 0 then (index, xs, inputs) else (i + 3, xs, inputs)
changeList inputs (JumpIfFalse bool index) xs i = if bool == 0 then (index, xs, inputs) else (i + 3, xs, inputs)
changeList inputs (LessThan x y index) xs i = if x < y then (i + 4, l1 ++ [1] ++ l2, inputs) else (i + 4, l1 ++ [0] ++ l2, inputs)
    where (l1, _:l2) = splitAt index xs
changeList inputs (Equals x y index) xs i = if x == y then (i + 4, l1 ++ [1] ++ l2, inputs) else (i + 4, l1 ++ [0] ++ l2, inputs)
    where (l1, _:l2) = splitAt index xs

isExit Exit = True
isExit _ = False

getInstruction :: Comp -> Int -> Instruction
getInstruction xs i =
    let (opCode, modes) = getBasicInstruction (xs !! i)
    in getInstruction' opCode modes xs i

getInstruction' :: Int -> [Int] -> Comp -> Int -> Instruction
getInstruction' 99 _ _ _ = Exit
getInstruction' 1 (m1:m2:_) xs i = Addition (getVal m1 xs (xs !! (i + 1))) (getVal m2 xs (xs !! (i+2))) (xs !! (i + 3))
getInstruction' 2 (m1:m2:_) xs i = Multiplication (getVal m1 xs (xs !! (i + 1))) (getVal m2 xs (xs !! (i+2))) (xs !! (i + 3))
getInstruction' 3 _ xs i = Input (xs !! (i + 1))
getInstruction' 4 _ xs i = Output (xs !! (i + 1))
getInstruction' 5 (m1:m2:_) xs i = JumpIfTrue (getVal m1 xs (xs !! (i + 1))) (getVal m2 xs (xs !! (i + 2)))
getInstruction' 6 (m1:m2:_) xs i = JumpIfFalse (getVal m1 xs (xs !! (i + 1))) (getVal m2 xs (xs !! (i + 2)))
getInstruction' 7 (m1:m2:_) xs i = LessThan (getVal m1 xs (xs !! (i + 1))) (getVal m2 xs (xs !! (i + 2))) (xs !! (i + 3))
getInstruction' 8 (m1:m2:_) xs i = Equals (getVal m1 xs (xs !! (i + 1))) (getVal m2 xs (xs !! (i + 2))) (xs !! (i + 3))

getVal :: Int -> Comp -> Int -> Int
getVal 0 xs val = xs !! val
getVal 1 _ val = val


getBasicInstruction :: Int -> (Int, [Int])
getBasicInstruction x = (opCode, modes)
    where opCode = read $ reverse $ take 2 str :: Int
          modes = map digitToInt (drop 2 str)
          str = reverse (show x) ++ ['0','0'..]

