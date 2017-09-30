import Sudoku
import Data.STRef
import Control.Monad.ST

main :: IO ()
main = putStrLn "Test suite not yet implemented"

test1 = do
    stm <- newSTMatrix 1 2
    x <- newSTRef 0
    writeSTMatrix stm 0 0 x
    writeSTMatrix stm 0 1 x
    m <- unsafeFreezeSTMatrix stm
    modifySTRef' x (+1)
    readSTRefMatrix m

--rows columns board
testBoard1 = (2, 3,
    ["--36--",
     "-2---4",
     "5---6-",
     "-3---5",
     "3---1-",
     "--14--"])

testSudoku1 = runST $ solveSudoku =<< makeMSudoku rows columns board
    where (rows, columns, board) = testBoard1
