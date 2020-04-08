import Data.List.Split

main = do
    input <- readFile "day8.txt"
    let rows = filter (\x -> x /= "\n") (chunksOf (25*6) input)
    let (_, row) = myMin rows
    let tot = (lengthChar '1' row) * (lengthChar '2' row)
    putStrLn $ show tot
    let joined = joinLists rows
    let color = map determineColor (joinLists rows)
    putStrLn $ stringify $ chunksOf (25) $ color

stringify :: [String] -> String
stringify (x:xs) = x ++ "\n" ++ stringify xs
stringify [] = ""


myMin :: [String] -> (Int, String)
myMin [x] = (lengthChar '0' x, x)
myMin (x:xs) = let (minZeros, minX) = myMin xs in
    if lengthX <= minZeros then (lengthX, x) else (minZeros, minX)
        where lengthX = lengthChar '0' x

joinLists :: [String] -> [String]
joinLists xs 
    | length (xs !! 1) == 0 = []
    | otherwise = map head xs : joinLists (map tail xs)

lengthChar c = length . filter ((==) c)

determineColor :: String -> Char
determineColor ('1':xs) = '0'
determineColor ('0':xs) = ' '
determineColor ('2':xs) = determineColor xs