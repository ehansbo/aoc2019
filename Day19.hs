import AocComputer
import qualified Data.Set as S

type Coord = (Int, Int)

main :: IO ()
main = do
    program <- compile "day19.txt"
    let tractors = filter (isTargeted program) [(x, y) | y <- [0..49], x <- [0..49]]
    --putStrLn $ show $ length tractors
    --putStrLn $ draw tractors
    square <- findSquare 100 program
    putStrLn $ show square

findSquare :: Int -> Program -> IO [Coord]
findSquare size program = findSquare' 3 3 4 S.empty
    where findSquare' xMin xMax y tractorPoints = do
            --putStrLn $ "now in findSquare with xMin = " ++ show xMin ++ ", xMax = " ++ show xMax ++ ", y = " ++ show y 
            case getSquare size (xMin, y) tractorPoints of
                Just xs -> return xs
                Nothing -> findSquare' xMin' xMax' y' (S.union tractorPoints $ S.fromList [(x, y') | x <- [xMin'..xMax']])
            where (xMin',xMax') = getNewRow y' xMin xMax program
                  y' = y + 1

getNewRow :: Int -> Int -> Int -> Program -> (Int, Int)
getNewRow row xMin xMax program
    | isTargeted program (xMin, row) = (xMin, getNewRow' (xMax, row))
    | otherwise = getNewRow row (xMin + 1) xMax program
    where getNewRow' (x, y)
            | isTargeted program (x+1, y) = getNewRow' (x + 1, y)
            | otherwise = x

getSquare :: Int -> Coord ->  S.Set Coord -> Maybe [Coord]
getSquare size (xMin, yMax) coords = 
    let (xMax, yMin) = (xMin + size - 1, yMax - size + 1)
    in if (xMax, yMin) `S.member` coords then Just [(xMin, yMin), (xMax, yMin), (xMin, yMax), (xMax, yMax)] else Nothing

isTargeted :: Program -> Coord -> Bool
isTargeted program (x, y) = head (fst $ runProgram program [x,y]) == 1

draw :: [Coord] -> String
draw ((x1,y1):(x2,y2):xs)
    | y2 == y1 = '#':(draw $ (x2,y2):xs)
    | otherwise = "#\n" ++ show y2  ++ ([' ' | _ <- [1..x2]]) ++ draw ((x2,y2):xs)
draw _ = "#"