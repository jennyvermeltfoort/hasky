import System.Console.ANSI
import System.IO (stdout)

import Data.Char
import Prelude hiding (map)

x = 50
y = 50

allCells = [(a,b) | a <- [0 .. (x - 1)], b <- [0 .. (y - 1)]]

neighbours (x0, y0) = [boundaryMap(a, b) | a <- [(x0 - 1) .. (x0 + 1)], b <- [(y0 - 1) .. (y0 + 1)], (a, b) /= (x0, y0)]
boundaryMap (a, b) = ((boundaryDigit a), (boundaryDigit b))
boundaryDigit a | a > (x - 1) = 0
                | a < 0 = (x - 1)
                | otherwise = a

numAliveNeighbours alive (a, b) = length (filter alive (neighbours (a, b)))

nextAlive alive (a, b) | numAliveNeighbours alive (a, b) == 3 = True
                | alive (a, b) && numAliveNeighbours alive (a, b) == 2 = True
                | otherwise = False

userCells :: [(Float, Float)]
userCells = [(1,1), (1,2), (1,3)]

nextState cells = filter (nextAlive (\n -> elem n cells)) allCells

printBoard :: [(Float, Float)] -> IO ()
printBoard [] = return ()
printBoard (x: state) = do
    print x
    printBoard state
    

printLoop cells = do
    let c = nextState cells
    printBoard c
    printLoop c


main = do
    stdoutSupportsANSI <- hNowSupportsANSI stdout
    setTitle "Life"

    printLoop userCells


    
