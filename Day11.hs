import AocComputer
import Data.Char
import Data.List
import System.IO.Unsafe


type Position = (Int, Int)
type Direction = (Int, Int)
type ColorGrid = [((Int, Int), Color)]

data Color = Black | White deriving (Show, Eq)
data Robot = Robot Position Direction


main :: IO ()
main = do
    program <- compile "./day11.txt" 
    run program
    run2 program

run :: Program -> IO ()
run program = do
  let robot = Robot (0, 0) (0, -1)
  let grid = []
  grid' <- runRobot robot grid program
  putStrLn $ show $ length grid'

run2 :: Program -> IO ()
run2 program = do
  let robot = Robot (0, 0) (0, -1)
  let grid = [((0,0), White)]
  grid' <- runRobot robot grid program
  let drawn = getDrawn grid'
  putStrLn $ getString grid' drawn

getString grid ((x,y):(x',y'):poss)
  | y' /= y = getDrawnInput (getInput (x,y) grid) ++ "\n" ++ getString grid ((x', y'):poss)
  | otherwise = getDrawnInput (getInput (x,y) grid) ++ getString grid ((x', y'):poss)
getString _ _ = ""

getDrawnInput 1 = "#"
getDrawnInput 0 = " "

getDrawn grid = 
  let xMin = getMinX grid 0
      yMin = getMinY grid 0
      xMax = getMaxX grid 0
      yMax = getMaxY grid 0 in do
        y <- [yMin..yMax]
        x <- [xMin..xMax]
        return (x, y)


getMinX (((x, _), _):grids) xMin
  | x < xMin = getMinX grids x
  | otherwise = getMinX grids xMin
getMinX [] xMin = xMin
getMinY (((_, y), _):grids) yMin
  | y < yMin = getMinY grids y
  | otherwise = getMinY grids yMin
getMinY [] yMin = yMin
getMaxX (((x, _), _):grids) xMax
  | x > xMax = getMaxX grids x
  | otherwise = getMaxX grids xMax
getMaxX [] xMax = xMax
getMaxY (((_, y), _):grids) yMax
  | y > yMax = getMaxY grids y
  | otherwise = getMaxY grids yMax
getMaxY [] yMax = yMax

runRobot :: Robot -> ColorGrid -> Program -> IO (ColorGrid)
runRobot robot@(Robot pos dir) grid program =
  let inputs = [getInput pos grid]
      (output, program') = runProgram program inputs in
    if isFinished program' then 
      return $ changeGrid grid pos (output !! 1)
    else
      runRobot (moveRobot robot (output !! 0)) (changeGrid grid pos (output !! 1)) program'



changeGrid :: ColorGrid -> Position -> Int -> ColorGrid
changeGrid ((posGrid, color):xs) pos output
  | posGrid == pos = (posGrid, getColor output):xs
  | otherwise = (posGrid, color):(changeGrid xs pos output)
changeGrid [] pos output = [(pos, getColor output)]

getColor 1 = White
getColor 0 = Black

moveRobot :: Robot -> Int -> Robot
moveRobot (Robot pos dir) output = Robot (move pos dir') dir'
  where dir' = changeDir dir output

move (x, y) (dirx, diry) = (x + dirx, y + diry)

changeDir :: Direction -> Int -> Direction
changeDir (1, 0) 1 = (0, 1)
changeDir (1, 0) 0 = (0, -1)
changeDir (0, 1) 1 = (-1, 0)
changeDir (0, 1) 0 = (1, 0)
changeDir (-1, 0) 1 = (0, -1)
changeDir (-1, 0) 0 = (0, 1)
changeDir (0, -1) 1 = (1, 0)
changeDir (0, -1) 0 = (-1, 0)


getInput :: Position -> ColorGrid -> Int
getInput _ [] = 0
getInput pos ((other, c):poss)
  | pos == other = getInput' c
  | otherwise = getInput pos poss 
    where getInput' c
            | c == Black = 0
            | c == White = 1
