main = do
	putStrLn $ show $ getCoordinatesSingle (1, 0) (1, 10)
	putStrLn $ show $ getCoordinatesSingle (1, 10) (1, 0)
	putStrLn $ show $ removeDuplicates [1,2,3,3,4,5,6,3]

removeDuplicates (x:y:xs)
    | x == y = x:(removeDuplicates xs)
    | otherwise = x:(removeDuplicates (y:xs))
removeDuplicates (x:[]) = [x]

test :: [(Int, Int)]
test = do
	a <- [1..1]
	b <- [10..1]
	return (a, b)

getCoordinatesSingle ::(Int, Int) -> (Int, Int) -> [(Int, Int)]
getCoordinatesSingle (x1, y1) (x2, y2)
    | x1 > x2 || y1 > y2 = reverse $ getCoordinatesSingle (x2, y2) (x1, y1)
	| otherwise = do
		x <- [x1..x2]
		y <- [y1..y2]
		return (x, y)