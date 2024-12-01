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
        bombCells = [(1,1), (0,0), (0,1)],
        flagCells = [(1,1)],
        openCells = [],
        dead = False
}

width :: Int
width = 10
height :: Int
height = 10

neighborCells :: (Int, Int) -> [(Int, Int)]
neighborCells (x,y) = [(a, b) | a <- [x-1 .. x+1], b <- [y-1 .. y+1], (a,b) /= (x,y), a >= 0, a < width, b >= 0, b < height]

numNeighborBomb :: ((Int, Int) -> Bool) -> (Int, Int) -> Int
numNeighborBomb isBombCell (x,y) = length (filter isBombCell (neighborCells (x,y)))
allCells = [(x,y) | x <- [0 .. width-1], y <- [0.. height-1]]

{-
addCells :: [(Int, Int)] -> ((Int, Int) -> Bool) -> ((Int, Int) -> Bool) -> [(Int, Int)] -> [(Int, Int)]
addCells openCells isFlag isBomb (c: cells) 
                                            | elem c openCells = openCells 
                                            | isFlag c = openCells
                                            | numNeighborBomb isBomb c == 0 = addCells (openCells ++ [c]) isFlag isBomb (neighborCells c)
                                            | cells == [] = openCells ++ [c]
                                            | otherwise = addCells (openCells ++ [c]) isFlag isBomb cells
-}

addCells isNotOpen isFlag isBomb (c: cells) 
                                            | cells == [] && (not (isNotOpen c) || isFlag c) = []
                                            | not (isNotOpen c) || isFlag c = addCells isNotOpen isFlag isBomb cells
                                            | numNeighborBomb isBomb c == 0 = 
                                                        trace ("hell" ++ show (neighborCells c))
                                                        [c] ++ addCells (\n -> not (elem n (neighborCells c))) isFlag isBomb (filter isNotOpen (neighborCells c))
                                            | cells == [] = [c]
                                            | otherwise = [c] ++ addCells isNotOpen isFlag isBomb cells

neighborZeroCells isBomb cell = [ c | c <- neighborCells cell, numNeighborBomb isBomb c == 0 ]
allNeighborZeroCells :: ((Int, Int) -> Bool) -> [(Int, Int)] -> [(Int, Int)]
allNeighborZeroCells isBomb (c: cells) | cells == [] = neighborZeroCells isBomb c
				       | otherwise = neighborZeroCells isBomb c ++ allNeighborZeroCells isBomb cells
                                  

{-
openState :: World -> (Int, Int) -> World
openState state (x,y) = World {
        bombCells = (bombCells state),
        flagCells = (flagCells state),
        openCells = addCell (openCells state) (\n -> elem n (flagCells state)) (\n -> elem n (bombCells state)) (x,y),
        dead = elem (x,y) (bombCells state)
}
-}

main :: IO ()
main = do
	print $ allNeighborZeroCells (\n -> elem n (bombCells baseState)) [(3,3)]
        --print $ numNeighborBomb (\n -> elem n (bombCells baseState)) (3,3)
        --print $ addCells (\n -> not (elem n (openCells baseState))) (\n -> elem n (flagCells baseState)) (\n -> elem n (bombCells baseState)) [(3,3)]
        --print $ (numNeighborBomb (\n -> elem n (bombCells baseState)) (3,3))
        --print $ (openCells (openState (openState baseState (3,3)) (1,2)))

