--import System.Console.ANSI
--import System.IO (stdout)

import Data.Char
import Data.List
import Data.Set
import Debug.Trace
import Prelude hiding (map)

data World = World
  { bombCells :: [(Int, Int)],
    flagCells :: [(Int, Int)],
    openCells :: [(Int, Int)],
    dead :: Bool
  }

width = 5 :: Int

height = 5 :: Int

neighborCells :: (Int, Int) -> [(Int, Int)]
neighborCells (x, y) = [(a, b) | a <- [x -1 .. x + 1], b <- [y -1 .. y + 1], (a, b) /= (x, y), a >= 0, a < width, b >= 0, b < height]

numNeighborBomb :: ((Int, Int) -> Bool) -> (Int, Int) -> Int
numNeighborBomb isBombCell (x, y) = length (filter isBombCell (neighborCells (x, y)))

openZeroCells :: ((Int, Int) -> Bool) -> ((Int, Int) -> Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
openZeroCells isFlag numNeighborBomb cell visited
  | isFlag cell = visited
  | elem cell visited = visited
  | numNeighborBomb cell /= 0 = cell : visited
  | otherwise =
    let newVisited = cell : visited
        neighbors = neighborCells cell
     in Prelude.foldl (\acc neighbor -> openZeroCells isFlag numNeighborBomb neighbor acc) newVisited neighbors

openCell :: ((Int, Int) -> Bool) -> ((Int, Int) -> Int) -> (Int, Int) -> [(Int, Int)]
openCell isFlag numNeighborBomb cell
  | isFlag cell = []
  | numNeighborBomb cell == 0 = openZeroCells isFlag numNeighborBomb cell []
  | otherwise = [cell]

baseState :: World
baseState =
  World
    { bombCells = [(2, 0), (2, 1), (2, 2)],
      flagCells = [(1, 1)],
      openCells = [],
      dead = False
    }

allCells = [(x, y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]

zeroBombCells numNeighborBomb = [x | x <- allCells, numNeighborBomb x == 0]

cellIsNeighbor a b = elem b (neighborCells a)

cellCreateNeighborRelation a b
  | cellIsNeighbor a b = [(a, b)]
  | otherwise = []

cellCreateCellNeighborRelations cells headCell
  | cells == [] = []
  | otherwise =
    let nextCells = tail cells
        firstCell = head cells
        relation = cellCreateNeighborRelation headCell firstCell
     in relation ++ cellCreateCellNeighborRelations nextCells headCell

cellCreateAllNeighborRelations zeroCells
  | zeroCells == [] = []
  | otherwise =
    let firstCell = head zeroCells
        nextCells = tail zeroCells
        relations = (cellCreateCellNeighborRelations nextCells firstCell)
     in relations ++ cellCreateAllNeighborRelations nextCells

main :: IO ()
main =
  do
    let zeroCells = zeroBombCells (numNeighborBomb (\n -> elem n (bombCells baseState)))
    print $ cellCreateAllNeighborRelations zeroCells

openState :: World -> (Int, Int) -> World
openState state cell =
  let newOpenCells = openCell (\n -> elem n (flagCells baseState)) (numNeighborBomb (\n -> elem n (bombCells baseState))) cell
      newDead =
        if dead state
          then dead state
          else elem cell (bombCells state)
   in World
        { bombCells = bombCells state,
          flagCells = flagCells state,
          openCells = openCells state ++ newOpenCells,
          dead = newDead
        }

intToChar :: Int -> Char
intToChar n
  | n >= 0 && n <= 9 = chr (n + 48) -- 48 is the ASCII code for '0'
  | otherwise = error "Input must be a single digit (0-9)"

cellChar state cell
  | elem cell (openCells state) && elem cell (bombCells state) = 'x'
  | elem cell (openCells state) = intToChar (numNeighborBomb (\n -> elem n (bombCells baseState)) cell)
  | elem cell (flagCells state) = 'f'
  | otherwise = '.'

grid state = [cellChar state cell | cell <- allCells]

printGrid (x : grid) index
  | grid == [] = putChar x
  | index < width = do
    putChar x
    printGrid grid (index + 1)
  | otherwise = do
    putChar '\n'
    putChar x
    printGrid grid 1
