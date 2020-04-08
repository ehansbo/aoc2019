import qualified Data.Set as S
import Data.List.Split
import Data.MemoTrie

type Point = (Int, Int)
data Bug = Alive | Dead deriving (Eq)
type Grid = Point -> Bug
type RecursiveGrid = Level -> Point -> Bug
type Level = Int

instance Show Bug where
    show Alive = "#"
    show Dead = "."

main :: IO ()
main = do
    input <- readFile "day24.txt"
    let bugs = parseInitialGrid $ filter (/= []) $ lines input
    let score = getFirstRepeat bugs S.empty
    putStrLn $ show score
    let bugsRecursive = \l -> if l == 0 then bugs else allDead
    let totalBugs = bugsAt 200 bugsRecursive
    putStrLn $ show totalBugs
    return ()


allDead :: Grid
allDead _ = Dead

bugsAt :: Int -> RecursiveGrid -> Int
bugsAt 0 grid = countBugsRec grid
bugsAt i grid = bugsAt (i - 1) (evolveRec grid)

evolveRec :: RecursiveGrid -> RecursiveGrid
evolveRec grid = memo2 go
    where 
      go lev (x, y)
        | x < 0  = Dead
        | y < 0  = Dead
        | y >= 5 = Dead
        | x >= 5 = Dead
        | x == 2 && y == 2 = Dead
        | grid lev (x, y) == Alive = if neighbors == 1 then Alive else Dead
        | grid lev (x, y) == Dead  = if neighbors == 1 || neighbors == 2 then Alive else Dead
            where neighbors = neighborsRec grid lev (x, y)


neighborsRec :: RecursiveGrid -> Level -> Point -> Int
neighborsRec grid level (x, y) = length $ filter (== Alive) (neighborsSame ++ neighborsUpper ++ neighborsLower)
    where 
      neighborsSame = [grid level (x+1, y), grid level (x-1, y), grid level (x, y+1), grid level (x, y-1)]
      neighborsUpper = neighborsUpperX ++ neighborsUpperY
      neighborsUpperX
        | x == 4 = [grid (level + 1) (3, 2)]
        | x == 0 = [grid (level + 1) (1, 2)]
        | otherwise = []
      neighborsUpperY
        | y == 4 = [grid (level + 1) (2, 3)]
        | y == 0 = [grid (level + 1) (2, 1)]
        | otherwise = []
      neighborsLower
        | (x, y) == (2, 1) = map (grid (level - 1)) [(x', 0) | x' <- [0..4]]
        | (x, y) == (2, 3) = map (grid (level - 1)) [(x', 4) | x' <- [0..4]]  
        | (x, y) == (1, 2) = map (grid (level - 1)) [(0, y') | y' <- [0..4]]
        | (x, y) == (3, 2) = map (grid (level - 1)) [(4, y') | y' <- [0..4]]
        | otherwise = []

countBugsRec :: RecursiveGrid -> Int
countBugsRec rec = sum $ map (countBugs . \i -> rec i) [-200..200]

countBugs :: Grid -> Int
countBugs grid = countBugs' (map grid [(x, y) | y <- [0..4], x <- [0..4]])
    where countBugs' [] = 0
          countBugs' (Alive:xs) = 1 + countBugs' xs
          countBugs' (Dead:xs) = countBugs' xs

parseInitialGrid :: [String] -> Grid
parseInitialGrid ls = memo go
  where
    maxY = length ls
    go (x, y)
      | x < 0     = Dead
      | y < 0     = Dead
      | y >= maxY    = Dead
      | x >= length (ls !! y) = Dead
      | (ls !! y) !! x == '.' = Dead
      | otherwise = Alive

getFirstRepeat :: Grid -> S.Set Int -> Int
getFirstRepeat grid previous
    | S.member score previous = score
    | otherwise =
        getFirstRepeat (evolve grid) (S.insert score previous)
            where score = getScore grid

evolve :: Grid -> Grid
evolve grid = memo go
    where
      go (x, y)
        | x < 0                = Dead
        | y < 0                = Dead
        | y >= 5               = Dead
        | x >= 5               = Dead
        | grid (x, y) == Alive = if neighbors grid (x, y) == 1 then Alive else Dead
        | grid (x, y) == Dead  = if neighbors grid (x, y) == 1 || neighbors grid (x, y) == 2 then Alive else Dead

neighbors :: Grid -> Point -> Int
neighbors grid (x, y) = length $ filter (== Alive) [grid (x+1, y), grid (x-1, y), grid (x, y+1), grid (x, y-1)]

getScore :: Grid -> Int
getScore grid = getScore' (map grid [(x, y) | y <- [0..4], x <- [0..4]]) 1
    where getScore' [] acc = 0
          getScore' (Alive:xs) acc = acc + getScore' xs acc*2
          getScore' (Dead:xs) acc = getScore' xs acc*2

drawGrid :: Grid -> String
drawGrid grid = unlines $ map concat $ chunksOf 5 $ map (show . grid) [(x, y) | y <- [0..4], x <- [0..4]]

