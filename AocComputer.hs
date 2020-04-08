module AocComputer (Program, Output, compile, runProgram, isFinished) where
import Data.Char
import DayZero

type Val = Int
type Index = Int
type Code = [Int]
type Output = [Int]
type RelativeBase = Int

data Instruction = 
  Multiplication Val Val Index
  | Addition Val Val Index
  | Exit
  | Input Index
  | Output Val
  | JumpIfTrue Val Index
  | JumpIfFalse Val Index
  | LessThan Val Val Index
  | Equals Val Val Index
  | ChangeBase Val deriving (Show)

data Program = Unfinished Code Index RelativeBase | Finished

instance Show Program where
  show Finished = "Finished"
  show (Unfinished c i r) = "Unfinished{index=" ++ show i ++ ", relativeBase=" ++ show r ++ "}"

compile' :: Code -> Program
compile' code = Unfinished code 0 0

compile :: String -> IO (Program)
compile fileName = do
  code <- fmap (++ [0,0..]) $ fileIntegers fileName ","
  return $ compile' code

isFinished Finished = True
isFinished _ = False

runProgram :: Program -> [Int] -> (Output, Program)
runProgram (Unfinished xs i base) inputs = do 
    let instruction = getInstruction xs base i
        (i', base', xs', inputs') = changeList inputs instruction xs base i 
        (output, continue) = runProgram (Unfinished xs' i' base') inputs' in
      case instruction of
        Exit -> ([], Finished)
        Output val -> (val : output, continue)
        Input index -> case inputs of
          [] -> ([], Unfinished xs i base)
          _ -> (output, continue)
        _ -> (output, continue)


changeList :: [Int] -> Instruction -> Code -> RelativeBase -> Int -> (Int, RelativeBase, [Int], [Int])
changeList inputs (Multiplication x y index) xs base i = (i + 4, base, l1 ++ [(x * y)] ++ l2, inputs)
    where (l1, _:l2) = splitAt index xs
changeList inputs (Addition x y index) xs base i = (i + 4, base, l1 ++ [(x + y)] ++ l2, inputs)
    where (l1, _:l2) = splitAt index xs
changeList (input:inputs) (Input index) xs base i = (i + 2, base, l1 ++ [input] ++ l2, inputs)
    where (l1, _:l2) = splitAt index xs
changeList inputs (JumpIfTrue bool index) xs base i  = if bool /= 0 then (index, base, xs, inputs) else (i + 3, base, xs, inputs)
changeList inputs (JumpIfFalse bool index) xs base i = if bool == 0 then (index, base, xs, inputs) else (i + 3, base, xs, inputs)
changeList inputs (LessThan x y index) xs base i = if x < y then (i + 4, base, l1 ++ [1] ++ l2, inputs) else (i + 4, base, l1 ++ [0] ++ l2, inputs)
    where (l1, _:l2) = splitAt index xs
changeList inputs (Equals x y index) xs base i = if x == y then (i + 4, base, l1 ++ [1] ++ l2, inputs) else (i + 4, base, l1 ++ [0] ++ l2, inputs)
    where (l1, _:l2) = splitAt index xs
changeList inputs (ChangeBase base') xs base i = (i + 2, base' + base, xs, inputs)
changeList inputs (Output _) xs base i = (i + 2, base, xs, inputs)

isExit Exit = True
isExit _ = False

getInstruction :: Code -> RelativeBase -> Index -> Instruction
getInstruction xs base i =
    let (opCode, modes) = getBasicInstruction (xs !! i)
    in getInstruction' opCode modes xs base i

getInstruction' :: Int -> [Int] -> Code -> RelativeBase -> Int -> Instruction
getInstruction' 99 _ _ _ _ = Exit
getInstruction' 1 (m1:m2:m3:_) xs base i = Addition (getVal m1 xs base (xs !! (i + 1))) (getVal m2 xs base (xs !! (i+2))) (getIndex m3 base (xs !! (i + 3)))
getInstruction' 2 (m1:m2:m3:_) xs base i = Multiplication (getVal m1 xs base (xs !! (i + 1))) (getVal m2 xs base (xs !! (i+2))) (getIndex m3 base (xs !! (i + 3)))
getInstruction' 3 (m:_) xs base i = Input (getIndex m base (xs !! (i + 1)))
getInstruction' 4 (m:_) xs base i = Output (getVal m xs base (xs !! (i + 1)))
getInstruction' 5 (m1:m2:_) xs base i = JumpIfTrue (getVal m1 xs base (xs !! (i + 1))) (getVal m2 xs base (xs !! (i + 2)))
getInstruction' 6 (m1:m2:_) xs base i = JumpIfFalse (getVal m1 xs base (xs !! (i + 1))) (getVal m2 xs base (xs !! (i + 2)))
getInstruction' 7 (m1:m2:m3:_) xs base i = LessThan (getVal m1 xs base (xs !! (i + 1))) (getVal m2 xs base (xs !! (i + 2))) (getIndex m3 base (xs !! (i + 3)))
getInstruction' 8 (m1:m2:m3:_) xs base i = Equals (getVal m1 xs base (xs !! (i + 1))) (getVal m2 xs base (xs !! (i + 2))) (getIndex m3 base (xs !! (i + 3)))
getInstruction' 9 (m:_) xs base i = ChangeBase (getVal m xs base (xs !! (i + 1)))

getVal :: Int -> Code -> RelativeBase -> Int -> Int
getVal 0 xs base val = xs !! val
getVal 1 _ _ val = val
getVal 2 xs base val = xs !! (val + base)

getIndex :: Int -> RelativeBase -> Int -> Index
getIndex 0 base val = val
getIndex 2 base val = base + val


getBasicInstruction :: Int -> (Int, [Int])
getBasicInstruction x = (opCode, modes)
    where opCode = read $ reverse $ take 2 str :: Int
          modes = map digitToInt (drop 2 str)
          str = reverse (show x) ++ ['0','0'..]
