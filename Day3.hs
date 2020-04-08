import Data.List
import Data.List.Split

main :: IO ()
main = do
    input <- readFile "day3.txt"
    let filtered = filter (\x -> x /= "") $ splitOn "\n" input
    let parsed = map ((map parse) . (splitOn ",")) filtered
    let line1 = removeDuplicates $ getCoordinates (parsed !! 0)
    let line2 = removeDuplicates $ getCoordinates (parsed !! 1)
    let (_:zipped1) = (zip [0..] line1)
    let (_:zipped2) = (zip [0..] line2)
    --putStrLn $ show $ minimum $ map distance1 $ intersect line1 line2
    putStrLn $ show $ minimum $ myIntersect zipped1 zipped2


removeDuplicates (x:y:xs)
    | x == y = x:(removeDuplicates xs)
    | otherwise = x:(removeDuplicates (y:xs))
removeDuplicates (x:[]) = [x]

distance1 :: (Int, Int, Int) -> Int
distance1 (_, a, b) = abs a + abs b

distance2 :: (Int, (Int, Int)) -> Int
distance2 (steps, _) = steps

myIntersect :: [(Int, (Int, Int))] -> [(Int, (Int, Int))] -> [Int]
myIntersect xs ys = myIntersect' ys xs ys

myIntersect' _ [] _ = []
myIntersect' base (x:xs) [] = myIntersect' base xs base
myIntersect' base ((stepsa, coordsa):xs) ((stepsb, coordsb):ys)
	| coordsa == coordsb = (stepsa + stepsb) : rest
	| otherwise = rest
		where rest = myIntersect' base ((stepsa, coordsa):xs) ys

parse :: String -> (Char, Int)
parse (x:xs) = (x, read xs)

getCoordinates :: [(Char, Int)] -> [(Int, Int)]
getCoordinates = getCoordinates' (0, 0)

getCoordinates' :: (Int, Int) -> [(Char, Int)] -> [(Int, Int)]
getCoordinates' _ [] = []
getCoordinates' (x, y) ((dir, distance):xs) = (getCoordinatesSingle (x, y) end) ++ getCoordinates' end xs
	where end = getEnd (x, y) distance dir

getCoordinatesSingle ::(Int, Int) -> (Int, Int) -> [(Int, Int)]
getCoordinatesSingle (x1, y1) (x2, y2)
    | x1 > x2 || y1 > y2 = reverse $ getCoordinatesSingle (x2, y2) (x1, y1)
	| otherwise = do
        x <- [x1..x2]
        y <- [y1..y2]
        return (x, y)
	

getEnd :: (Int, Int) -> Int -> Char -> (Int, Int)
getEnd (x, y) distance 'U' = (x, y + distance)
getEnd (x, y) distance 'R' = (x + distance, y)
getEnd (x, y) distance 'L' = (x - distance, y)
getEnd (x, y) distance 'D' = (x, y - distance)
