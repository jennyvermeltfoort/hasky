--import System.Console.ANSI
--import System.IO (stdout)

import Data.Char
import Data.List as List
import Data.Set as Set hiding (cartesianProduct)
import Debug.Trace
import Prelude hiding (map)

data World = World
  { bombCells :: Set.Set (Int, Int),
    flagCells :: Set.Set (Int, Int),
    openCells :: Set.Set (Int, Int),
    dead :: Bool
  }

width = 5 :: Int

height = 5 :: Int

neighborCells :: (Int, Int) -> [(Int, Int)]
neighborCells (x, y) = [(a, b) | a <- [x -1 .. x + 1], b <- [y -1 .. y + 1], (a, b) /= (x, y), a >= 0, a < width, b >= 0, b < height]

numNeighborBomb :: ((Int, Int) -> Bool) -> (Int, Int) -> Int
numNeighborBomb isBombCell (x, y) = length (Prelude.filter isBombCell (neighborCells (x, y)))

baseState :: World
baseState =
  World
    { bombCells = Set.fromList [(2, 0)],
      flagCells = Set.fromList [],
      openCells = Set.fromList [],
      dead = False
    }

allCells = [(x, y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]

zeroBombCells numNeighborBomb isBomb = [x | x <- allCells, numNeighborBomb x == 0, not (isBomb x)]

neighborCellsBottomRight :: (Int, Int) -> [(Int, Int)]
neighborCellsBottomRight (x, y) = [(x + 1, y), (x, y + 1)]

cellIsNeighbor a b = elem b (neighborCellsBottomRight a)

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

cartesianProduct :: [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
cartesianProduct a b = [(a0, b1) | (a0, a1) <- a, (b0, b1) <- b]

inverseRelation :: [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
inverseRelation a = [(a1, a0) | (a0, a1) <- a]

makeTransitive :: [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
makeTransitive a =
  Set.toList (Set.unions [(Set.fromList a), (makeTransitive' a 1), (makeTransitive' a 2)])
  where
    makeTransitive' a 0 = Set.fromList (cartesianProduct a a)
    makeTransitive' a num =
      let product = Set.fromList (cartesianProduct a a)
          next = makeTransitive' a (num - 1)
       in Set.union product next

makeSymmetric :: [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
makeSymmetric a = a ++ inverseRelation a

main :: IO ()
main =
  do
    let isBomb = (\n -> member n (bombCells baseState))
    let zeroCells = zeroBombCells (numNeighborBomb isBomb) isBomb
    let relations = cellCreateAllNeighborRelations zeroCells
    let symmetric = makeSymmetric relations
    let paths = Set.fromList (makeTransitive symmetric)
    print $ List.groupBy (\a b -> member (a, b) paths) zeroCells
