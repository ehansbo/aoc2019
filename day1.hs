import DayZero

main :: IO ()
main = do
    lines <- fileIntegers "./input1.txt" "\n"
    let totalFuel = foldl (+) 0 (map fuel2 lines)
    putStrLn $ show totalFuel

fuel1 :: Integer -> Integer
fuel1 x = x `div` 3 - 2

fuel2 :: Integer -> Integer
fuel2 x 
    | y < 1 = 0
    | otherwise = y + fuel2 y
        where y = x `div` 3 - 2 
