import Data.List.Split
import Data.List

type Coord = (Int, Int)
type Cluster = (Quadrant, Double, [Coord])

data Quadrant = TR | BR | BL | TL deriving (Show, Eq)

main :: IO ()
main = do
    input <- readFile "day10.txt"
    let filtered = filter (\x -> x /= "") $ splitOn "\n" input
    let parsed = parseFile filtered
    let (len, coord) = maximumBy (\(x,_) (y,_) -> compare x y) $ getVisible parsed
    putStrLn $ show len
    let other = filter ((/=) coord) parsed
    let destructionOrder = destroy $ cluster (shift coord other) []
    putStrLn $ show $ (shiftBack coord destructionOrder) !! 199

shift :: Coord -> [Coord] -> [Coord]
shift (x, y) ((x1, y1):coords) = (x1 - x, y1 - y):(shift (x, y) coords)
shift _ [] = [] 

shiftBack :: Coord -> [Coord] -> [Coord]
shiftBack (x, y) ((x1, y1):coords) = (x1 + x, y1 + y):(shiftBack (x, y) coords)
shiftBack _ [] = [] 


parseFile :: [String] -> [Coord]
parseFile = parseFile' (0, 0)
    where parseFile' (x, y) (('#':chars):rows) = (x, y) : (parseFile' (x + 1, y) (chars:rows))
          parseFile' (x, y) (('.':chars):rows) = parseFile' (x + 1, y) (chars:rows)
          parseFile' (x, y) ([]:rows) = parseFile' (0, y + 1) rows
          parseFile' _ [] = []


getVisible :: [Coord] -> [(Int, Coord)]
getVisible xs = getVisible' xs xs
    where getVisible' all (x:xs) = (getVisible'' x all []) : getVisible' all xs
          getVisible' _ [] = []
          getVisible'' coord [] currentNeighbors = (length currentNeighbors, coord)
          getVisible'' coord (possibleNeighbor:xs) currentNeighbors = if coord /= possibleNeighbor && allowedNeighbor coord possibleNeighbor currentNeighbors then
            getVisible'' coord xs (possibleNeighbor:currentNeighbors) else getVisible'' coord xs currentNeighbors

allowedNeighbor coord possibleNeighbor (currentNeighbor:all) = not (isBlocker coord possibleNeighbor currentNeighbor) && allowedNeighbor coord possibleNeighbor all
allowedNeighbor _ _ _ = True

isBlocker (x1, y1) (x2, y2) (x3, y3) = not ((x2 < x1 && x3 > x1) || (x2 > x1 && x3 < x1) || (y2 < y1 && y3 > y1) || (y2 > y1 && y3 < y1)) && (x3 - x1) * (y2 - y1) == (y3 - y1) * (x2 - x1)

destroy :: [Cluster] -> [Coord]
destroy clustered = if empty allQs then allDestroyed else allDestroyed ++ (destroy allQs)
    where d1 = destroyQuadrant (-1.0) (filterQuadrant TR clustered)
          d2 = destroyQuadrant (-1.0) (filterQuadrant BR clustered)
          d3 = destroyQuadrant (-1.0) (filterQuadrant BL clustered)
          d4 = destroyQuadrant (-1.0) (filterQuadrant TL clustered)
          allDestroyed = d1 ++ d2 ++ d3 ++ d4
          allQs = removeCoords allDestroyed clustered

removeCoords :: [Coord] -> [Cluster] -> [Cluster]
removeCoords coords clusters = filter (not . emptyCluster) $ map (removeCoords' coords) clusters

removeCoords' :: [Coord] -> Cluster -> Cluster
removeCoords' (c:cs) (q, a, cCoords) = removeCoords' cs (q, a, delete c cCoords)
removeCoords' [] cluster = cluster

emptyCluster :: Cluster -> Bool
emptyCluster (_, _, []) = True
emptyCluster _ = False

filterQuadrant :: Quadrant -> [Cluster] -> [Cluster]
filterQuadrant q ((q', d, c):clusters)
    | q == q' = (q', d, c):(filterQuadrant q clusters)
    | otherwise = filterQuadrant q clusters
filterQuadrant _ _ = []

destroyQuadrant :: Double -> [Cluster] -> [Coord]
destroyQuadrant angle clusters = let minCluster = getMinCluster angle clusters in
 case minCluster of
    Nothing -> []
    Just (q, angle', coords) -> minCoord : destroyQuadrant angle' clusters
        where minCoord = getMinCoords coords

getMinCluster angle clusters 
    | filtered == [] = Nothing
    | otherwise = Just $ minimumBy (\(_, a1, _) (_, a2, _) -> compare a1 a2) $ filtered
        where filtered = filter (\(_, a, _) -> a > angle) clusters
        

getMinCoords coords = minimumBy (\c1 c2 -> compare (lengthCoord c1) (lengthCoord c2)) coords

empty (x:xs) = False
empty [] = True

cluster :: [Coord] -> [Cluster] -> [Cluster]
cluster (coord:coords) clusters = cluster coords (addToCluster coord clusters)
cluster [] clusters = clusters

addToCluster :: Coord -> [Cluster] -> [Cluster]
addToCluster coord [] = [(quadrant, getAngle coord quadrant, [coord])]
    where quadrant = getQuadrant coord
addToCluster coord ((q, a, (other:others)):pcs)
    | isBlocker (0, 0) coord other = (q, a, coord:other:others):pcs
    | otherwise = (q, a, (other:others)) : addToCluster coord pcs

getAngle coord TR = angle (0, -1) coord 
getAngle coord BR = angle (1, 0) coord 
getAngle coord BL = angle (0, 1) coord 
getAngle coord TL = angle (-1, 0) coord 


getQuadrant :: Coord -> Quadrant
getQuadrant (x, y)
    | x >= 0 && y < 0 = TR
    | x >= 0 && y >= 0 = BR
    | x < 0 && y >= 0 = BL
    | x < 0 && y < 0 = TL

scalar :: Coord -> Coord -> Int
scalar (x1, y1) (x2, y2) = x1*x2 + y1*y2

lengthCoord :: Coord -> Double
lengthCoord (x, y) = sqrt(fromIntegral (x^2) + fromIntegral (y^2))

angle :: Coord -> Coord -> Double
angle c1 c2 = acos $ (fromIntegral $ scalar c1 c2) / ((lengthCoord c1) * (lengthCoord c2)) 