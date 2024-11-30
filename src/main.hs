import Data.Char
import Prelude hiding (map)


func num xs = xs ++ [num]

list :: Int -> Int -> [Int]
list n x = take n (repeat x)

get_cell_char :: Int -> Char
get_cell_char n =
    if n == 0 then
        ' '
    else
        'x'   

world_print_aux :: [Int] -> Int -> Int -> IO ()
world_print_aux [] _ _ = return ()
world_print_aux (x: world) l n = do
    if n < l then do
        putChar $ get_cell_char x
        world_print_aux world l (n+1)
    else do
        putChar '\n'
        putChar $ get_cell_char x
        world_print_aux world l 1

world_print :: [Int] -> Int -> IO ()
world_print world l = world_print_aux world l 0

main :: IO ()
main = do
    let width = 10
    let height = 10
    let world = list (width*height) 1

    world_print world width


    
