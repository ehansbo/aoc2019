import DayZero
import Data.Char
import AocComputer
import qualified Data.Set as S
import Data.List
import Data.List.Split
import Control.Concurrent
import qualified Control.Monad as M


type Coord = (Int, Int)

data OutputStatus = Done | Valid | Invalid | Visited



main :: IO ()
main = do
    program <- compile "./day15.txt"
    run program
    
  
run :: Program -> IO ()
run program = do
  let (state, oxCoord, distance) = head $ getValidDistances ([1], program) S.empty (0, 0) 0
  putStrLn $ show $ distance
  let last = getInvalidDistances ([1], state) S.empty oxCoord 0
  putStrLn $ show $ maximum $ map snd last


getValidDistances :: (Output, Program) -> S.Set Coord -> Coord -> Int -> [(Program, Coord, Int)]
getValidDistances (out, state) visited current@(currentX, currentY) currentLength = 
  let outputStatus = getOutputStatus out visited current
  in case outputStatus of
      Done -> [(state, current, currentLength)]
      Invalid -> []
      Visited -> []
      Valid ->
        let east = getValidDistances (runProgram state [4]) (S.insert current visited) (currentX + 1, currentY) (currentLength + 1)
            west = getValidDistances (runProgram state [3]) (S.insert current visited) (currentX - 1, currentY) (currentLength + 1)
            north = getValidDistances (runProgram state [1]) (S.insert current visited) (currentX, currentY - 1) (currentLength + 1)
            south = getValidDistances (runProgram state [2]) (S.insert current visited) (currentX, currentY + 1) (currentLength + 1)
        in east ++ west ++ north ++ south 

getInvalidDistances :: (Output, Program) -> S.Set Coord -> Coord -> Int -> [(Coord, Int)]
getInvalidDistances (out, state) visited current@(currentX, currentY) currentLength = 
  let outputStatus = getOutputStatus out visited current
  in case outputStatus of
      Done -> []
      Invalid -> [(current, currentLength - 1)]
      Visited -> []
      Valid ->
        let east = getInvalidDistances (runProgram state [4]) (S.insert current visited) (currentX + 1, currentY) (currentLength + 1)
            west = getInvalidDistances (runProgram state [3]) (S.insert current visited) (currentX - 1, currentY) (currentLength + 1)
            north = getInvalidDistances (runProgram state [1]) (S.insert current visited) (currentX, currentY - 1) (currentLength + 1)
            south = getInvalidDistances (runProgram state [2]) (S.insert current visited) (currentX, currentY + 1) (currentLength + 1)
        in east ++ west ++ north ++ south 


getOutputStatus :: Output -> S.Set Coord -> Coord ->  OutputStatus
getOutputStatus [0] _ _ = Invalid
getOutputStatus [1] visited coord
  | coord `S.member` visited = Visited
  | otherwise = Valid
getOutputStatus [2] _ _ = Done


