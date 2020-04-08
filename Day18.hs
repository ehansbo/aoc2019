
import Data.List.Split
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.PQueue.Min as Q
import qualified Data.Set as S
import qualified Data.Map as M
type Position = (Int, Int)
type Key = Char
type Door = Char

type MinPaths = M.Map (Key, Key) (Int, [Door])

data Grid = Grid {
    positions :: [Position]
  , walls :: S.Set Position
  , keys :: M.Map Position Key
  , doors :: M.Map Position Door
} deriving (Show)

data MapState = MapState {
    position :: Position
  , distanceTraveled :: Int
  , visited :: S.Set Position
} deriving (Show)

newGrid = Grid [] S.empty M.empty M.empty


instance Eq MapState where
    a == b = (distanceTraveled a) == (distanceTraveled b)

instance Ord MapState where
    compare a b = compare (distanceTraveled a) (distanceTraveled b)


main :: IO ()
main = do
    input1 <- readFile "./day18.txt"
    input2 <- readFile "./day18b.txt"
    putStr "Day 18a: "
    run input1
    putStr "Day 18b: "
    run input2

run :: String -> IO ()
run input = do
    let filtered = filter (\x -> x /= []) $ splitOn "\n" input
    let grid = initialize filtered
    let positionTags = zip (positions grid) ['1'..'9']
    let minPaths = map fromJust $ filter isJust $ map (getMinPath grid) $ allPairs $ positionTags ++ (M.toList $ keys grid)

    let minPathMap = toMap M.empty $ addReverse minPaths

    mini <-  getKeyOrder (Q.singleton (0, map snd positionTags, [], minPathMap)) S.empty
    putStrLn $ show $ mini

getKeyOrder :: Q.MinQueue (Int, [Key], [Door], M.Map Key [(Key, Int, [Door])]) -> S.Set ([Key], [Door]) -> IO (Int)
getKeyOrder queue alreadyVisited = if length validPaths == 0 then return currentLength else
    if S.member ((sort currentKeys), (sort openedDoors)) alreadyVisited 
        then getKeyOrder (Q.deleteMin queue) alreadyVisited else
             getKeyOrder (Q.deleteMin $ (foldl (\q s -> Q.insert s q) queue) validPaths) (S.insert ((sort currentKeys), (sort openedDoors)) alreadyVisited)
                where validPaths = map (\(currentKey, (nextKey, nextLength, _)) -> (nextLength + currentLength, nextKey:(delete currentKey currentKeys), sort (nextKey:openedDoors), deleteFromMap currentKey keyMap)) 
                                    $ filter (\(_, (_, _, doors)) -> length (filter (\door -> (not $ door `elem` openedDoors)) doors) == 0) possiblePaths {- Filter out paths that lead to unopened doors -}
                      possiblePaths = foldl (\xs currentKey -> (zip [currentKey,currentKey..] $ fromJustOrEmpty $ M.lookup currentKey keyMap) ++ xs) [] currentKeys
                      (currentLength, currentKeys, openedDoors, keyMap) = Q.findMin queue

fromJustOrEmpty :: Maybe [a] -> [a]
fromJustOrEmpty Nothing = []
fromJustOrEmpty (Just x) = x


deleteFromMap :: Key -> M.Map Key [(Key, Int, [Door])] -> M.Map Key [(Key, Int, [Door])]
deleteFromMap key keyMap = M.map (\xs -> filter (\(key', _, _) -> key' /= key) xs) $ M.delete key keyMap


toMap :: M.Map Key [(Key, Int, [Door])] -> [((Key, Key), (Int, [Door]))] -> M.Map Key [(Key, Int, [Door])]
toMap map (((k1, k2), (distance, doors)):xs) = toMap (addToList map k1 (k2, distance, doors)) xs
toMap map _ = map

addToList :: Ord a => M.Map a [b] -> a -> b -> M.Map a [b]
addToList map key value = case M.lookup key map of
    Nothing -> M.insert key [value] map
    Just values -> M.insert key (value:values) map


addReverse :: [((a, a), (c, d))] -> [((a, a), (c, d))]
addReverse (((k1, k2), (i, d)):xs) = ((k1, k2), (i, d)):((k2, k1), (i, d)):(addReverse xs)
addReverse [] = []

getMinPath :: Grid -> ((Position, Key), (Position, Key)) -> Maybe ((Key, Key), (Int, [Door]))
getMinPath grid ((p, k1), (_, k2)) = 
    case shortestToKey (Q.singleton ((MapState p 0 S.empty), [])) k2 grid of
        Just ((MapState _ distanceTraveled _), visitedDoors) -> Just ((k1, k2), (distanceTraveled, visitedDoors))
        Nothing -> Nothing

allPairs :: [a] -> [(a, a)]
allPairs (x:xs) = (map (\y -> (x, y)) xs) ++ allPairs xs
allPairs [] = []


shortestToKey :: Q.MinQueue (MapState, [Door]) -> Key -> Grid -> Maybe (MapState, [Door])
shortestToKey queue key grid 
    | Q.size queue == 0 = Nothing
    | otherwise =
        if M.lookup (position state) (keys grid) == Just key then Just (state, doors) else
            shortestToKey (Q.deleteMin $ foldl (\q s -> Q.insert s q) queue $ filter (validFollowup grid) (getFollowups state' doors')) key grid
            where (state, doors) = Q.findMin queue 
                  queue' = Q.insert (state', doors') queue
                  state' = addVisited state
                  doors' = case maybeDoor of
                    Just door' -> door':doors 
                    Nothing -> doors 
                  maybeDoor = getDoor grid (position state)

getDoor :: Grid -> Position -> Maybe Door
getDoor (Grid _ _ _ doors) pos = M.lookup pos doors

validFollowup :: Grid -> (MapState, [Door]) -> Bool
validFollowup grid ((MapState pos _ visited), _)
    | S.member pos (walls grid) = False
    | S.member pos visited = False
    | otherwise = True

getFollowups :: MapState -> [Door] -> [(MapState, [Door])]
getFollowups state visitedDoors = map (getFollowups' state visitedDoors) [(1,0),(-1,0),(0,1),(0,-1)]
    where getFollowups' (MapState (x, y) distanceTraveled visited) visitedDoors (xP, yP) = (MapState pos' (distanceTraveled + 1) visited, visitedDoors)
            where x' = x + xP
                  y' = y + yP
                  pos' = (x + xP, y + yP)

initialize :: [String] -> Grid
initialize = (initialize' newGrid) . zipCoordinates
    where initialize' state [] = state
          initialize' state ((_, []):ss) = initialize' state ss
          initialize' state ((y, ((x, c):cs)):ss) = initialize' (update c state) ((y, cs):ss)
            where update c (Grid pos walls keys doors) = case c of
                    '#' -> Grid pos (S.insert (x, y) walls) keys doors
                    '@' -> Grid ((x, y):pos) walls keys doors
                    _
                      | c `elem` ['a'..'z'] -> Grid pos walls (M.insert (x, y) c keys) doors
                      | c `elem` ['A'..'Z'] -> Grid pos walls keys (M.insert (x, y) (toLower c) doors)
                      | otherwise -> state

addVisited :: MapState -> MapState
addVisited (MapState pos distanceTraveled visited) = MapState pos distanceTraveled (S.insert pos visited)

zipCoordinates :: [String] -> [(Int, [(Int, Char)])]
zipCoordinates ss = zip [0..] (map (zip [0..]) ss)

