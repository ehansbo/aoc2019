import Data.MemoTrie

data Punkt = Punkt {x :: Int, y :: Int, namn :: String} deriving Show

data Polylinje = Polylinje {bredd :: Int, farg :: String, horn :: [Punkt]} deriving Show

main :: IO ()
main = do
    let p1 = Polylinje 3 "röd" [Punkt 1 2 "p1", Punkt 3 5 "p2", Punkt 5 9 "p3"]
    putStrLn $ show p1
    putStrLn $ farg p1
    putStrLn $ show $ avstand (Punkt 0 1 "apa") (Punkt 1 0 "bepa")
    putStrLn $ show $ langd p1

langd :: Polylinje -> Double
langd poly = langd' (horn poly)
    where langd' (p1:p2:ps) = avstand p1 p2 + langd' (p2:ps)
          langd' _ = 0

avstand :: Punkt -> Punkt -> Double
avstand (Punkt x1 y1 _) (Punkt x2 y2 _) = sqrt $ fromIntegral $ (x2 - x1)^2 + (y2 - y1)^2

primes :: [Integer]
primes = 2:filterPrime [3,5..]
    where filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]