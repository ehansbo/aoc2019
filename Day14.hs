import DayZero
import qualified Data.Map as Map
import Data.List.Split

type Chemical = String
type Ingredient = (Int, Chemical)
type Leftovers = Map.Map Chemical Int
type Requirements = Map.Map Chemical (Int, [Ingredient])

main :: IO ()
main = do
    input <- fmap (toRequirements . (filter $ \x -> x /= "") . (splitOn "\n"))  $ readFile "day14.txt"
    let (ore, _) = getOre 1 "FUEL" input Map.empty
    putStrLn $ "Part 1: " ++ show ore
    putStrLn $ "Part 2: " ++ (show $ run2 input 1 (10^13))

run2 :: Requirements -> Int -> Int -> Int
run2 req lower upper
    | (upper - lower) <= 1 =
        let (created, _) = getOre upper "FUEL" req Map.empty
        in if created > 1000000000000 then lower else upper
    | otherwise =
        let middle = (upper + lower) `div` 2
            (created, _) = getOre middle "FUEL" req Map.empty
        in if created > 1000000000000 then run2 req lower middle else run2 req middle upper


getOre :: Int -> Chemical -> Requirements -> Leftovers -> (Int, Leftovers)
getOre num "ORE" _ left = (num, left)
getOre num chem req left = 
    let num' = num - (lookupOr chem left 0)
        (created, needed) = totalNeeded num' $ alwaysLookup chem req
        (ore, left') = 
            foldl 
            (\(oreAcc, leftAcc) (numNeeded, chemNeeded) -> let (newOre, newLeft) = getOre numNeeded chemNeeded req leftAcc in (oreAcc + newOre, newLeft))
            (0, left) needed
    in (ore, Map.insert chem (created - num') left')

totalNeeded :: Int -> (Int, [Ingredient]) -> (Int, [Ingredient])
totalNeeded needed (created, ingredients) = 
    let totalNeeded = ceiling $ fromIntegral needed / fromIntegral created
    in (created * totalNeeded, map (\(i, chem) -> (i*totalNeeded, chem)) ingredients)

lookupOr key map def = case Map.lookup key map of
    Nothing -> def
    Just v -> v

alwaysLookup key map = case Map.lookup key map of
    Nothing -> error "Unknown key in map"
    Just v -> v

toRequirements :: [String] -> Requirements
toRequirements = Map.fromList . map toRequirement

toRequirement :: String -> (Chemical, (Int, [Ingredient]))
toRequirement s = 
    let (inputs:output:[]) = splitOn " => " s
        (outputNum, outputChem) = toIngredient output
        inputs' = map toIngredient $ splitOn ", " inputs
    in (outputChem, (outputNum, inputs'))

toIngredient :: String -> Ingredient
toIngredient s = (read first, second)
    where (first:second:[]) = splitOn " " s