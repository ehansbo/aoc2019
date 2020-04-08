import AocComputer
import Data.Char
import Data.List.Split
import Data.List

main :: IO ()
main = do
    putStrLn $ show $ splitOn "," totalInstr
    putStrLn $ show $ getPaths totalInstr
    --program <- compile "./day17.txt"
    --draw $ fst $ runProgram program []
    --program2 <- compile "./day17b.txt"
    --draw2 $ fst $ runProgram program2 $ instr ++ movA ++ movB ++ movC ++ shouldPrint
  
getPaths :: String -> [[String]]
getPaths s = 
    let possiblePaths = getPaths' 0 2 [] $ splitOn "," s
        stringified = map (map (intercalate ",")) possiblePaths
    in filter (\x -> length (filter (\y -> length y < 20) x) == 3) stringified



getPaths' :: Int -> Int -> [[String]] -> [String] -> [[[String]]]
getPaths' 3 _ prev [] = [prev]
getPaths' 3 _ prev _ = []
getPaths' num size prev ss 
    | size < length ss = getPaths' (num + 1) 2 (prev ++ [taken]) (removeFromList taken ss) ++ getPaths' num (size+2) prev ss
    | otherwise = []
        where taken = take size ss

removeFromList :: Eq a => [a] -> [a] -> [a]
removeFromList xs ys = concat $ filter (/=[]) (splitOn xs ys)


draw2 :: Output -> IO ()
draw2 output = do
    putStrLn $ foldl (\x y -> x ++ [chr y]) "" output
    putStrLn $ show $ last output

draw :: Output -> IO ()
draw output = do
  let str = foldl (\x y -> x ++ [chr y]) "" output
  let intersections = getIntersections (filter (\x -> length x > 0) (splitOn "\n" str)) (1,1)
  putStrLn $ show $ foldl (\acc (x, y) -> acc + x*y) 0 intersections

getIntersections :: [String] -> (Int, Int) -> [(Int, Int)]
getIntersections lines (x, y)
    | y >= (length lines) - 1 = []
    | x >= (length $ lines !! y) - 1 = getIntersections lines (1, y + 1)
    | isIntersection lines x y = (x, y) : getIntersections lines (x + 1, y)
    | otherwise = getIntersections lines (x + 1, y)

isIntersection :: [String] -> Int -> Int -> Bool
isIntersection lines x y = 
    (lines !! y) !! x == '#' 
    && (lines !! (y - 1)) !! x == '#' 
    && (lines !! (y + 1)) !! x == '#' 
    && (lines !! y) !! (x - 1) == '#' 
    && (lines !! y) !! (x + 1) == '#'

toAscii :: String -> [Int]
toAscii = map ord

instr = toAscii "B,C,B,A,C,B,A,C,B,A\n"
movA = toAscii "L,12,L,6,R,12,R,8\n"
movB = toAscii "L,12,L,12,L,6,L,6\n"
movC = toAscii "R,8,R,4,L,12\n"
shouldPrint = toAscii "n\n"

totalInstr = "L,12,L,12,L,6,L,6,R,8,R,4,L,12,L,12,L,12,L,6,L,6,L,12,L,6,R,12,R,8,R,8,R,4,L,12,L,12,L,12,L,6,L,6,L,12,L,6,R,12,R,8,R,8,R,4,L,12,L,12,L,12,L,6,L,6,L,12,L,6,R,12,R,8"