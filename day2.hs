import DayZero

main :: IO ()
main = do
    elements <- fileIntegers "./day2.txt" ","
    putStrLn $ show $ numberTwo elements

numberTwo :: [Int] -> (Int, Int)
numberTwo (s:noun:verb:rest)
    | calculateList (s:noun:verb:rest) 0 !! 0 == 19690720 = (noun, verb)
    | verb == 128 = numberTwo $ s:(noun + 1):0:rest
    | otherwise = numberTwo $ s:noun:(verb + 1):rest

calculateList :: [Int] -> Int -> [Int]
calculateList xs i
    | xs !! i == 99 = xs
    | xs !! i == 1 = calculateList (changeList (+) xs a1 a2 p) (i + 4)
    | xs !! i == 2 = calculateList (changeList (*) xs a1 a2 p) (i + 4)
    where a1 = (xs !! (xs !! (i + 1)))
          a2 = (xs !! (xs !! (i + 2)))
          p  = (xs !! (i + 3))


changeList :: (Int -> Int -> Int) -> [Int] -> Int -> Int -> Int -> [Int]
changeList op xs a1 a2 p = l1 ++ [val] ++ l2
    where val = op a1 a2
          (l1, _:l2) = splitAt p xs