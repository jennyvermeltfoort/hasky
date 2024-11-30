import Data.Char
import Prelude hiding (map)

add :: Int -> Int
add x = x + 5


fact :: Int -> Int
fact n | n == 0 = 1
       | n /= 0 = n * fact(n-1)

roots :: (Float, Float, Float) -> (Float, Float)
roots (a, b, c) = (x1, x2) where
    x1 = e + sqrt d / (2 * a)
    x2 = e - sqrt d / (2 * a)
    d = b * b - 4 * a * c
    e = - b / (2 * a)

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map func (x: abc) = func x : map func abc

main :: IO ()
main = do
    input1 <- getLine
    input2 <- getLine
    let y = (read input2 :: Int)
    let x = (read input1 :: Int)

    putStrLn "Hello, World!"
    print $ map fact [x .. y]
    

