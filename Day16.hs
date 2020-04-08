main :: IO ()
main = do
    input <- readFile "./day16.txt"

    putStrLn $ "Part 1: " ++ (take 8 $ run input)

    let offset = read $ take 7 input
    let input2 = drop offset $ concat $ replicate 10000 input
    putStrLn $ "Part 2: " ++ (take 8 $ run2 input2)

run :: String -> String
run input = calculateNumber input 100

run2 :: String -> String
run2 input = calculateNumber2 input 100

calculateNumber :: String -> Int -> String
calculateNumber s 0 = s
calculateNumber s i = calculateNumber (getNumber s 1) (i - 1)

calculateNumber2 :: String -> Int -> String
calculateNumber2 s 0 = s
calculateNumber2 s i = calculateNumber2 (getNumber2 s $ sumOfString s) (i - 1)

getNumber2 :: String -> Int -> String
getNumber2 (c:cs) sum = show (sum `mod` 10) ++ getNumber2 cs (sum - (read [c]))
getNumber2 [] 0 = []

sumOfString = sum . map (\x -> read [x])

getNumber :: String -> Int -> String
getNumber s repeats
    | repeats == length s + 1 = []
    | otherwise = show (abs number `mod` 10) ++ getNumber s (repeats + 1)
        where number = getNumber' s $ tail $ concat $ repeat  $ concat $ map (replicate repeats) [0,1,0,-1]

getNumber' :: String -> [Int] -> Int
getNumber' (c:cs) (ph:phs) = ((read [c]) * ph) + getNumber' cs phs
getNumber' [] _ = 0
