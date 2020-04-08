import Data.List.Split

type Pos = (Int, Int, Int)
type Speed = (Int, Int, Int)


main :: IO ()
main = do
    input <- readFile "day12.txt"
    let filtered = filter (\x -> x /= "") $ splitOn "\n" input
    let parsed = map parsePoint filtered
    putStrLn $ show $ parsed
    let withSpeed = zip parsed $ map (\_ -> (0, 0, 0)) parsed
    putStrLn $ show $ (energy withSpeed) !! 1000
    putStrLn $ show $ findRepeats withSpeed

findRepeats :: [(Pos, Speed)] -> Int
findRepeats xs = lcm' [px, py, pz]
    where px = findRepeats' (toX xs) (updateSingle (toX xs)) 1
          py = findRepeats' (toY xs) (updateSingle (toY xs)) 1
          pz = findRepeats' (toZ xs) (updateSingle (toZ xs)) 1
          findRepeats' orig pos num = if orig == pos then num else findRepeats' orig (updateSingle pos) (num + 1)

updateSingle :: [(Int, Int)] -> [(Int, Int)]
updateSingle xs = applyVelocitySingle xs'
    where xs' = applyGravitySingle xs xs

applyVelocitySingle :: [(Int, Int)] -> [(Int, Int)]
applyVelocitySingle [] = []
applyVelocitySingle ((x, sx):xs) = ((x + sx, sx):applyVelocitySingle xs)

applyGravitySingle :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
applyGravitySingle [] _ = []
applyGravitySingle (x:xs) all = applyGravitySingle' x (filter (/=x) all) : applyGravitySingle xs all
    where applyGravitySingle' (x, sx) ((x',_):xs) = applyGravitySingle' (x, sx + norm (x' - x)) xs
          applyGravitySingle' a [] = a


toX :: [(Pos, Speed)] -> [(Int, Int)]
toX xs = map (\((x, _, _), (sx, _, _)) -> (x, sx)) xs

toY :: [(Pos, Speed)] -> [(Int, Int)]
toY xs = map (\((_, y, _), (_, sy, _)) -> (y, sy)) xs

toZ :: [(Pos, Speed)] -> [(Int, Int)]
toZ xs = map (\((_, _, z), (_, _, sz)) -> (z, sz)) xs

lcm' :: [Int] -> Int
lcm' = foldl (\x y -> lcm x y) 1

energy :: [(Pos, Speed)] -> [Int]
energy xs = calculateEnergy xs : energy (update xs)

calculateEnergy :: [(Pos, Speed)] -> Int
calculateEnergy [] = 0
calculateEnergy (((x, y, z), (sx, sy, sz)):xs) = (abs x + abs y + abs z)*(abs sx + abs sy + abs sz) + (calculateEnergy xs)

update :: [(Pos, Speed)] -> [(Pos, Speed)]
update xs = applyVelocity xs'
    where xs' = applyGravity xs xs

applyVelocity :: [(Pos, Speed)] -> [(Pos, Speed)]
applyVelocity [] = []
applyVelocity (((x, y, z), speed@(sx, sy, sz)):xs) = ((x + sx, y + sy, z + sz), speed):(applyVelocity xs)

applyGravity :: [(Pos, Speed)] -> [(Pos, Speed)] -> [(Pos, Speed)]
applyGravity [] _ = []
applyGravity (x:xs) all = applyGravity' x (filter (/=x) all) : applyGravity xs all
    where applyGravity' (pos@(x, y, z), (sx, sy, sz)) (((x',y',z'),_):ys) = applyGravity' (pos, (sx + norm (x'-x), sy + norm (y'-y), sz + norm (z'-z))) ys
          applyGravity' a [] = a

norm a = if a == 0 then 0 else a `div` (abs a)

parsePoint :: String -> Pos
parsePoint s = let (x:y:z:_) = splitOn "," $ filter (\c -> not $ c `elem` "<xyz= >") s in (read x, read y, read z)