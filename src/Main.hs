--import System.Console.ANSI
--import System.IO (stdout)

import Data.Char
import Data.List as List
import Debug.Trace
import Prelude hiding (map)

type Cell = (Int, Int)

type CellRelation = (Cell, Cell)

data World = World
  { bombCells :: [Cell],
    flagCells :: [Cell],
    openCells :: [Cell],
    zeroCells :: [Cell],
    zeroCellsNeighbors :: [[Cell]],
    dead :: Bool,
    win :: Bool
  }

width :: Int
width = 20

height :: Int
height = 20

allCells :: [Cell]
allCells = [(x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]

neighborCells :: Cell -> [Cell]
neighborCells (x, y) = [(a, b) | b <- [y -1 .. y + 1], a <- [x -1 .. x + 1], (a, b) /= (x, y), a >= 0, a < width, b >= 0, b < height]

numNeighborBomb :: (Cell -> Bool) -> Cell -> Int
numNeighborBomb isBombCell (x, y) = length (filter isBombCell (neighborCells (x, y)))

zeroBombCells :: (Cell -> Int) -> (Cell -> Bool) -> [Cell]
zeroBombCells numNeighborBomb isBomb = [x | x <- allCells, numNeighborBomb x == 0, not (isBomb x)]

neighborCellsBottomRight :: Cell -> [Cell]
neighborCellsBottomRight (x, y) = [(x + 1, y), (x, y + 1)]

cellIsNeighbor :: Cell -> Cell -> Bool
cellIsNeighbor a b = elem b (neighborCellsBottomRight a)

cellCreateNeighborRelation :: Cell -> Cell -> [CellRelation]
cellCreateNeighborRelation a b
  | cellIsNeighbor a b = [(a, b)]
  | otherwise = []

cellCreateCellNeighborRelations :: [Cell] -> Cell -> [CellRelation]
cellCreateCellNeighborRelations cells headCell
  | cells == [] = []
  | otherwise =
    let nextCells = tail cells
        firstCell = head cells
        relation = cellCreateNeighborRelation headCell firstCell
     in relation ++ cellCreateCellNeighborRelations nextCells headCell

cellCreateAllNeighborRelations :: [Cell] -> [CellRelation]
cellCreateAllNeighborRelations zeroCells
  | zeroCells == [] = []
  | otherwise =
    let firstCell = head zeroCells
        nextCells = tail zeroCells
        relations = (cellCreateCellNeighborRelations nextCells firstCell)
     in relations ++ cellCreateAllNeighborRelations nextCells

cartesianProduct :: [CellRelation] -> [CellRelation] -> [CellRelation]
cartesianProduct a b = [(a0, b1) | (b0, b1) <- b, (a0, a1) <- a, a1 == b0]

inverseRelation :: [CellRelation] -> [CellRelation]
inverseRelation a = [(a1, a0) | (a0, a1) <- a]

makeTransitive :: [CellRelation] -> [CellRelation]
makeTransitive a =
  union a (makeTransitive' a a 2) -- R U R^1 U R^2
  where
    makeTransitive' a product 0 = product
    makeTransitive' a product num =
      let productN = cartesianProduct a product -- A X A = R ^ n
          next = makeTransitive' product productN (num - 1) -- R X R = R ^ n+1
       in union productN next

makeSymmetric :: [CellRelation] -> [CellRelation]
makeSymmetric a = union a (inverseRelation a)

partitionAddNeighbor :: [Cell] -> [Cell]
partitionAddNeighbor (c : part)
  | part == [] = neighbors
  | otherwise = union neighbors (partitionAddNeighbor part)
  where
    neighbors = union [c] (neighborCells c)

partitionAddNeighbors :: [[Cell]] -> [[Cell]]
partitionAddNeighbors (p : parts)
  | parts == [] = [partitionAddNeighbor p]
  | otherwise = [partitionAddNeighbor p] ++ partitionAddNeighbors parts

getZeroCellPartitions :: [Cell] -> [[Cell]]
getZeroCellPartitions zeroCells =
  let rel = cellCreateAllNeighborRelations zeroCells
      sRel = makeSymmetric rel
      tsRel = makeTransitive sRel
   in groupBy (\a b -> elem (a, b) tsRel) zeroCells

-- Create a partitioned list of zero cells and their neighbors.
getZeroCellPartitionsNeighbor :: [Cell] -> [[Cell]]
getZeroCellPartitionsNeighbor zeroCells = partitionAddNeighbors (getZeroCellPartitions zeroCells)

baseState :: World
baseState =
  World
    { bombCells = bombs,
      flagCells = [],
      openCells = [],
      zeroCells = zero,
      zeroCellsNeighbors = zeroNeighbors,
      dead = False,
      win = False
    }
  where
    bombs = [(0, 1), (4, 1)]
    isBomb = (\n -> elem n bombs)
    zero = zeroBombCells (numNeighborBomb isBomb) isBomb
    zeroNeighbors = getZeroCellPartitionsNeighbor zero

openCell :: (Cell -> Bool) -> (Cell -> Bool) -> (Cell -> Bool) -> [[Cell]] -> Cell -> [Cell]
openCell isOpen isFlag isZeroCell zeroCellsNeighbors cell
  | isFlag cell = []
  | isOpen cell = []
  | isZeroCell cell = concat (filter (\n -> elem cell n) zeroCellsNeighbors)
  | otherwise = [cell]

openState :: World -> Cell -> World
openState state cell =
  World
    { bombCells = bombCells state,
      flagCells = flagCells state,
      zeroCells = zero,
      zeroCellsNeighbors = zeroPart,
      openCells = newOpen,
      dead = dead state,
      win = newWin
    }
  where
    zero = zeroCells state
    zeroPart = zeroCellsNeighbors state
    isOpen = (\n -> elem n (openCells state))
    isFlag = (\n -> elem n (flagCells state))
    isZeroCell = (\n -> elem n zero)
    newOpen = (openCells state) ++ openCell isOpen isFlag isZeroCell zeroPart cell
    newDead = elem cell (bombCells state)
    newWin = elem cell (bombCells state)

intToChar :: Int -> Char
intToChar n
  | n >= 0 && n <= 9 = chr (n + 48) -- 48 is the ASCII code for '0'
  | otherwise = error "Input must be a single digit (0-9)"

cellChar state cell
  | elem cell (openCells state) && elem cell (bombCells state) = 'x'
  | elem cell (openCells state) = intToChar (numNeighborBomb (\n -> elem n (bombCells baseState)) cell)
  | elem cell (flagCells state) = 'f'
  | elem cell (bombCells state) = 'i'
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

main :: IO ()
main =
  do
    putChar '\n'
    let open = (openState baseState (3, 3))
    print $ zeroCellsNeighbors open
    printGrid (grid open) 0
    putChar '\n'
    putChar '\n'