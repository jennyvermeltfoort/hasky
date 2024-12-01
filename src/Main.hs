--import System.Console.ANSI
--import System.IO (stdout)

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

width = 4 :: Int
height = 4 :: Int

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


baseState :: World
baseState = World {
        bombCells = [(0,0), (0,1), (0,2), (0,3)],
        flagCells = [(1,1)],
        openCells = [],
        dead = False
}

openState :: World -> (Int, Int) -> World
openState state cell =
        let newOpenCells = openCell (\n -> elem n (flagCells baseState)) (numNeighborBomb (\n -> elem n (bombCells baseState))) cell
            newDead = if dead state 
                      then dead state
                      else elem cell (bombCells state)
        in World {
                bombCells = bombCells state,
                flagCells = flagCells state,
                openCells = openCells state ++ newOpenCells,
                dead = newDead
        }

intToChar :: Int -> Char
intToChar n
    | n >= 0 && n <= 9 = chr (n + 48)  -- 48 is the ASCII code for '0'
    | otherwise = error "Input must be a single digit (0-9)"

allCells = [(x,y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
cellChar state cell 
        | elem cell (openCells state) && elem cell (bombCells state) = 'x'
        | elem cell (openCells state) = intToChar (numNeighborBomb (\n -> elem n (bombCells baseState)) cell)
        | elem cell (flagCells state) = 'f'
        | otherwise = '.'
grid state = [cellChar state cell | cell <- allCells]

printGrid (x: grid) index
        | grid == [] = putChar x
        | index < width = do
                putChar x
                printGrid grid (index + 1)
        | otherwise = do
                putChar '\n'
                putChar x
                printGrid grid 1
                
main :: IO ()
main = do
        printGrid (grid (openState baseState (3,3))) 0
