import Data.List.Split

main :: IO ()
main = do
    input <- readFile "day6.txt"
    let filtered = filter (\x -> x /= "") $ splitOn "\n" input
    let parsed = map (parseLine . (splitOn ")")) filtered
    putStrLn $ show $ count "COM" 0 parsed parsed
    let l1 = find "SAN" "COM" parsed parsed
    let l2 = find "YOU" "COM" parsed parsed
    putStrLn $ show $ calc l1 l2

calc :: [String] -> [String] -> Int
calc (x:xs) (y:ys)
    | x == y = calc xs ys
    | otherwise = length xs + length ys 

count :: String -> Int -> [(String, String)] -> [(String, String)] -> Int
count parent val [] _ = val
count parent val ((p, c):xs) full
    | p == parent = count c (val + 1) full full + rest
    | otherwise = rest
        where rest = count parent val xs full

find :: String -> String -> [(String, String)] -> [(String, String)] -> [String]
find target parent [] _ 
    | parent == target = [parent]
    | otherwise = []
find target parent ((p, c):xs) full
    | p == parent && childResult /= [] = [parent] ++ childResult
    | otherwise = find target parent xs full
        where childResult = find target c full full

parseLine :: [String] -> (String, String)
parseLine (p:c:_) = (p, c)