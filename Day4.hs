main :: IO ()
main = do
	let input = [156218..652527]
	putStrLn $ show $ length $ filter validPassword2 (map show input)

validPassword1 :: String -> Bool
validPassword1 pwd = twoSame1 pwd && noDecrease pwd


validPassword2 :: String -> Bool
validPassword2 pwd = twoSame2 pwd && noDecrease pwd

twoSame1 :: String -> Bool
twoSame1 (x:y:xs) = x == y || twoSame1 (y:xs)
twoSame1 _ = False

twoSame2 :: String -> Bool
twoSame2 = twoSame2' 'X' 

twoSame2' :: Char -> String -> Bool
twoSame2' prev (x:y:z:xs) = (x == y && x /= z && x /= prev) || twoSame2' x (y:z:xs)
twoSame2' prev (x:y:xs) = x == y && x /= prev

noDecrease :: String -> Bool
noDecrease (x:y:xs) = not ((read [y] :: Int) < (read [x] :: Int)) && noDecrease (y:xs)
noDecrease _ = True