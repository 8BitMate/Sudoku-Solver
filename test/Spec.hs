import Sudoku
import Data.STRef

main :: IO ()
main = putStrLn "Test suite not yet implemented"

test = do
    stm <- newSTMatrix 1 2
    x <- newSTRef 0
    writeSTMatrix stm 0 0 x
    writeSTMatrix stm 0 1 x
    m <- unsafeFreezeSTMatrix stm
    modifySTRef' x (+1)
    readSTRefMatrix m
