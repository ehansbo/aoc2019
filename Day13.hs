import AocComputer
import Data.Char
import Data.List
import Data.List.Split
import Control.Concurrent
import qualified Control.Monad as M
import System.Console.ANSI
import System.IO

main :: IO ()
main = do
  program1 <- compile "./day13.txt"
  program2 <- compile "./day13b.txt"
  run program1
  run2 program2
  
run :: Program -> IO ()
run program = do
    let (out, _) = runProgram program []
    let grid = getGrid $ reverse out
    putStrLn $ show $ length $ filter (\(x,y,z) -> z == 2) grid

run2 :: Program -> IO ()
run2 program = do
  hSetBuffering stdout $ BlockBuffering (Just 3000000)
  let out = playGame $ runProgram program [0]
  
  let full = drawFull $ map getGrid out
  M.forM_ full $ \(score, s) -> do
    clearScreen
    putStrLn $ "SCORE: " ++ (show score) ++ "\n"  
    putStr s
    hFlush stdout
    threadDelay 1000

drawFull :: [[(Int, Int, Int)]] -> [(Int, String)]
drawFull out = drawFull' 0 (head out) out
  where drawFull' prevScore prev (this:xs) = 
          let (modified, s) = draw prev this 
              newScore = if (score this) == 0 then prevScore else score this 
          in (newScore, s) : drawFull' newScore modified xs
        drawFull' _ _ [] = []

draw :: [(Int, Int, Int)]-> [(Int, Int, Int)] -> ([(Int, Int, Int)], String)
draw prev out = (grid, drawSorted $ map (map (\(_, _, z) -> z)) $ getAndSort grid)
  where grid = join prev (filter (\(x, y, z) -> x >= 0) out)

getAndSort :: [(Int, Int, Int)] -> [[(Int, Int, Int)]]
getAndSort = getAndSort' 0
getAndSort' y xs = let forY = filter (\(_,y',_) -> y == y') xs in case forY of
  [] -> []
  ys -> (sort ys) : (getAndSort' (y + 1) xs)
 
getMaxX :: [(Int, Int, Int)] -> Int
getMaxX = maximum . (map (\(x, _, _) -> x))

join :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
join prev ((x,y,z):xs) = let splitted = splitWhen (\(x',y',_) -> x' == x && y' == y) prev in
  case splitted of
    (l1:l2:_) -> (join (l1 ++ [(x,y,z)] ++ l2) xs)
    nonSplitted -> (join ((x,y,z):prev) xs)
join prev [] = prev

drawSorted :: [[Int]] -> String
drawSorted (line:lines) = drawLine (tail line) ++ "\n" ++ drawSorted' lines
  where drawLine = map myGetChar
        drawSorted' (line:lines) = drawLine line ++ "\n" ++ drawSorted' lines
        drawSorted' [] = ""

myGetChar :: Int-> Char
myGetChar 0 = ' '
myGetChar 1 = '█' 
myGetChar 2 = '■'
myGetChar 3 = '▬'
myGetChar 4 = '●'


playGame :: (Output, Program) -> [Output]
playGame (out, program)
  | isFinished program = [reverse out]
  | otherwise =
      let joystickDirection = getJoystickDirection (reverse out) in
      (reverse out) : (playGame $ runProgram program [joystickDirection])    

getJoystickDirection :: Output -> Int
getJoystickDirection out =
  let (ball, _) = getElement 4 out
      (paddle, _) = getElement 3 out in
      --if(paddle > ball) then -1 else 0
      if ball < paddle
        then -1 
        else if ball > paddle then 1 
          else 0
      
getElement :: Int -> Output -> (Int, Int)
getElement element = head . map (\(x, y, z) -> (x, y)) . filter (\(x, y, z) -> z == element) . getGrid

score :: [(Int, Int, Int)] -> Int
score out = case map (\(x,y,z) -> z) $ filter (\(x, y, z) -> x == (-1) && y == 0) out of
  (x:xs) -> x
  [] -> 0

getGrid :: Output -> [(Int, Int, Int)]
getGrid (x:y:z:xs) = (x,y,z):(getGrid xs)
getGrid [] = []