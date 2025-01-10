--import System.Console.ANSI
--import System.IO (stdout)

import Data.Char (Char, chr)
import Data.List as List
import Debug.Trace ()
import Prelude hiding (map)

type Cell = (Int, Int)

type CellRelation = (Cell, Cell)

allCells :: [Cell]
allCells = [(x, y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]

neighbors :: Cell -> [Cell]
neighbors (x, y) = [(a, b) | b <- [y -1 .. y + 1], a <- [x -1 .. x + 1], (a, b) /= (x, y), a >= 0, a < width, b >= 0, b < height]

numNeighbors :: (Cell -> Bool) -> Cell -> Int
numNeighbors isBombCell (x, y) = length (filter isBombCell (neighbors (x, y)))

zeroCells :: (Cell -> Int) -> (Cell -> Bool) -> [Cell]
zeroCells numNeighbors' isBomb = [x | x <- allCells, numNeighbors' x == 0, not (isBomb x)]

isNeighbor :: Cell -> Cell -> Bool
isNeighbor (x, y) neighbor = neighbor `elem` [(x + 1, y), (x, y + 1)]

createPath' :: [Cell] -> [Cell] -> [CellRelation] -> [CellRelation]
createPath' [] _ acc = acc
createPath' (a : as) [] acc = createPath' as as (acc ++ [(a, a)])
createPath' listA listB acc
  | isNeighbor a b = createPath' as as (acc ++ [(a, b)])
  | otherwise = createPath' listA bs acc
  where
    (a : as) = listA
    (b : bs) = listB

createPath :: [Cell] -> [CellRelation]
createPath list = createPath' list list []

walkPathAndGroup' (rel:rels) accT accG
  | a /= b = walkPathAndGroup' rels accT (accG ++ a)
  | a == b = walkPathAndGroup' rels (accT : (accG ++ a)) [] 
  where (a, b) = rel

main :: IO ()
main =
  do
    let isBomb = (`elem` bombs baseState)
    let zero = zeroCells (numNeighbors isBomb) isBomb
    print $ zero
    print $ createPath zero

data World = World
  { bombs :: [Cell],
    flags :: [Cell],
    opens :: [Cell],
    zeros :: [Cell],
    dead :: Bool,
    win :: Bool
  }

baseState :: World
baseState =
  World
    { bombs = newBombs,
      flags = [],
      opens = [],
      zeros = newZeros,
      dead = False,
      win = False
    }
  where
    newBombs = [(2, 1)]
    isBomb = (`elem` newBombs)
    numNeighbors' = numNeighbors isBomb
    newZeros = zeroCells numNeighbors' isBomb

width :: Int
width = 5

height :: Int
height = 4

intToChar :: Int -> Char
intToChar n
  | n >= 0 && n <= 9 = chr (n + 48) -- 48 is the ASCII code for '0'
  | otherwise = error "Input must be a single digit (0-9)"

cellChar :: World -> Cell -> Char
cellChar state cell
  | cell `elem` opens state && cell `elem` bombs state = 'x'
  | cell `elem` opens state = intToChar (numNeighbors (`elem` bombs state) cell)
  | cell `elem` flags state = 'f'
  | cell `elem` bombs state = 'i'
  | otherwise = '.'

grid :: World -> [Char]
grid state = [cellChar state cell | cell <- allCells]

printGrid :: [Char] -> Int -> IO ()
printGrid (x : grid) index
  | null grid = putChar x
  | index < width = do
    putChar x
    printGrid grid (index + 1)
  | otherwise = do
    putChar '\n'
    putChar x
    printGrid grid 1
