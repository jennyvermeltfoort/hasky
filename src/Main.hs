import System.Console.ANSI
import System.IO (stdout)

import Data.Char
import Prelude hiding (map)
import Data.List

import Debug.Trace


data World = World {
        bombCells :: [(Int, Int)],
        flagCells :: [(Int, Int)],
        openCells :: [(Int, Int)],
        dead :: Bool
}

baseState :: World
baseState = World {
        bombCells = [(0,0), (0,1), (0,2), (0,3)],
        flagCells = [(1,1)],
        openCells = [],
        dead = False
}

width :: Int
width = 4
height :: Int
height = 4

neighborCells :: (Int, Int) -> [(Int, Int)]
neighborCells (x,y) = [(a, b) | a <- [x-1 .. x+1], b <- [y-1 .. y+1], (a,b) /= (x,y), a >= 0, a < width, b >= 0, b < height]

numNeighborBomb :: ((Int, Int) -> Bool) -> (Int, Int) -> Int
numNeighborBomb isBombCell (x,y) = length (filter isBombCell (neighborCells (x,y)))

openZeroCells :: ((Int, Int) -> Bool) -> ((Int, Int) -> Int)  -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
openZeroCells isFlag numNeighborBomb cell visited
    | isFlag cell = visited
    | elem cell visited = visited
    | numNeighborBomb cell /= 0 = cell : visited
    | otherwise = 
        let newVisited = cell : visited
            neighbors = neighborCells cell
        in foldl (\acc neighbor -> openZeroCells isFlag numNeighborBomb neighbor acc) newVisited neighbors 


openCell :: ((Int, Int) -> Bool) -> ((Int, Int) -> Int) -> (Int, Int) -> [(Int, Int)]
openCell isFlag numNeighborBomb cell 
        | isFlag cell = []
        | numNeighborBomb cell == 0 = openZeroCells isFlag numNeighborBomb cell []
        | otherwise = [cell]

openState :: World -> (Int, Int) -> World
openState state (x,y) = World {
        bombCells = (bombCells state),
        flagCells = (flagCells state),
        openCells = addCell (openCells state) (\n -> elem n (flagCells state)) (\n -> elem n (bombCells state)) (x,y),
        dead = elem (x,y) (bombCells state)
}

main :: IO ()
main = do
    print $ openCell (\n -> elem n (flagCells baseState)) (numNeighborBomb (\n -> elem n (bombCells baseState))) (3,3)


