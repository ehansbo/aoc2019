import Data.List.Split
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.PQueue.Min as Q

type Labyrinth = [String]
type Coord = (Int, Int)
type FromPortal = M.Map String [Coord]
type ToPortal = M.Map Coord String
type Visited = S.Set
type Level = Int


main :: IO ()
main = do
    input <- readFile "./day20.txt"
    let labyrinth = filter (\x -> x /= []) $ splitOn "\n" input
    let portals = getPortals labyrinth
    let startingPosition = head $ fromJust $ M.lookup "AA" portals
    putStrLn $ show $ getShortest (Q.singleton (0, startingPosition)) S.empty "ZZ" portals (reversePortal portals) labyrinth
    putStrLn $ show $ getShortestLevels (Q.singleton (0, startingPosition, 0)) S.empty "ZZ" portals (reversePortal portals) labyrinth
    return ()


reversePortal :: FromPortal -> ToPortal
reversePortal fromPortal = reversePortal' M.empty $ M.toList fromPortal
    where reversePortal' toPortal ((p, coords):xs) = reversePortal' (insertAll coords p toPortal) xs
          reversePortal' toPortal _ = toPortal

insertAll :: Ord k => [k] -> v -> M.Map k v -> M.Map k v
insertAll (k:ks) v map = insertAll ks v $ M.insert k v map
insertAll _ _ map = map

getShortest :: Q.MinQueue (Int, Coord) -> S.Set Coord -> String -> FromPortal -> ToPortal -> Labyrinth -> Int
getShortest queue visited endPortal fromPortal toPortal labyrinth =
    let (distance, position) = fromJust $ Q.getMin queue
        neighbors = getNeighbors position fromPortal toPortal labyrinth
        poppedQueue = Q.deleteMin queue
        updatedQueue = Q.union poppedQueue (Q.fromList (zip (repeat $ distance + 1) neighbors))
    in if M.lookup position toPortal == Just endPortal
        then distance 
        else if S.member position visited 
            then getShortest poppedQueue visited endPortal fromPortal toPortal labyrinth
            else getShortest updatedQueue (S.insert position visited) endPortal fromPortal toPortal labyrinth


getNeighbors :: Coord -> FromPortal -> ToPortal -> Labyrinth -> [Coord]
getNeighbors (x, y) fromPortal toPortal labyrinth = 
    let closeNeighbors = filter (\pos -> getCoord pos labyrinth == '.') [(x+1, y),(x-1, y),(x, y+1),(x, y-1)]
        portalNeighbor = M.lookup (x, y) toPortal
    in case portalNeighbor of
        Nothing -> closeNeighbors
        Just "AA" -> closeNeighbors
        Just "ZZ" -> closeNeighbors
        Just portal -> head (filter (\pos -> pos /= (x,y)) $ fromJust $ M.lookup portal fromPortal) : closeNeighbors


getShortestLevels :: Q.MinQueue (Int, Coord, Level) -> S.Set (Coord, Level) -> String -> FromPortal -> ToPortal -> Labyrinth -> Int
getShortestLevels queue visited endPortal fromPortal toPortal labyrinth =
    let (distance, position, level) = fromJust $ Q.getMin queue
        neighbors = getNeighborsLevels position level fromPortal toPortal labyrinth
        poppedQueue = Q.deleteMin queue
        updatedQueue = Q.union poppedQueue $ Q.fromList $ map (\(c, l) -> (distance + 1, l, c)) neighbors
    in if level == 0 && M.lookup position toPortal == Just endPortal
        then distance 
        else if S.member (position, level) visited 
            then getShortestLevels poppedQueue visited endPortal fromPortal toPortal labyrinth
            else getShortestLevels updatedQueue (S.insert (position, level) visited) endPortal fromPortal toPortal labyrinth



getNeighborsLevels :: Coord -> Level -> FromPortal -> ToPortal -> Labyrinth -> [(Level, Coord)]
getNeighborsLevels (x, y) level fromPortal toPortal labyrinth = 
    let closeNeighbors = zip (repeat level) $ filter (\pos -> getCoord pos labyrinth == '.') [(x+1, y),(x-1, y),(x, y+1),(x, y-1)]
        portalNeighbor = M.lookup (x, y) toPortal
    in case portalNeighbor of
        Nothing -> closeNeighbors
        Just "AA" -> closeNeighbors
        Just "ZZ" -> closeNeighbors
        Just portal -> filter (\(l, _) -> l >= 0) $ (getLevelCoord level labyrinth (head (filter (\pos -> pos /= (x,y)) $ fromJust $ M.lookup portal fromPortal))) : closeNeighbors

getLevelCoord :: Level -> Labyrinth -> Coord -> (Level, Coord)
getLevelCoord currentLevel lab (x, y)
    | x == 2 || y == 2 || y == (length lab) - 3 || x == (length $ head lab) - 3 = (currentLevel + 1, (x, y))
    | otherwise = (currentLevel - 1, (x, y))


getPortals :: Labyrinth -> FromPortal
getPortals lab = getPortals' (0,0) M.empty
    where getPortals' (x,y) portals
            | y >= length lab = portals
            | x >= length (lab !! y) = getPortals' (0, y+1) portals
            | otherwise = getPortals' (x+1, y) $ maybeAddPortal (x,y) lab portals

maybeAddPortal :: Coord -> Labyrinth -> FromPortal -> FromPortal
maybeAddPortal (x,y) lab portals
    | getCoord (x,y) lab == '.' = case maybePortal of
        Just portal -> M.insert portal ((x,y):(lookupList portal portals)) portals
        Nothing -> portals
    | otherwise = portals
        where maybePortal = getMaybePortal lab (x, y)

lookupList key map = case M.lookup key map of
    Just x -> x
    Nothing -> []

getMaybePortal :: Labyrinth -> Coord -> Maybe String
getMaybePortal lab (x,y) 
    | isPortal lab (x-1, y) (x-2, y) = Just $ sort [getCoord (x-2, y) lab, getCoord (x-1, y) lab]
    | isPortal lab (x+1, y) (x+2, y) = Just $ sort [getCoord (x+2, y) lab, getCoord (x+1, y) lab]
    | isPortal lab (x, y-1) (x, y-2) = Just $ sort [getCoord (x, y-2) lab, getCoord (x, y-1) lab]
    | isPortal lab (x, y+1) (x, y+2) = Just $ sort [getCoord (x, y+2) lab, getCoord (x, y+1) lab]
    | otherwise = Nothing

isPortal :: Labyrinth -> Coord -> Coord -> Bool
isPortal lab c1 c2 = getCoord c1 lab `elem` ['A'..'Z'] && getCoord c2 lab `elem` ['A'..'Z']

getCoord :: Coord -> Labyrinth -> Char
getCoord (x,y) lab = (lab !! y) !! x