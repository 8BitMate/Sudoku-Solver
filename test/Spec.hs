import Sudoku
import Data.STRef
import Control.Monad.ST

main :: IO ()
main = do
    putStrLn "solving testBoard1:\n-------------"
    mapM_ print testSudoku1
    putStrLn "-------------\n"

    putStrLn "solving testBoard2:\n-------------"
    mapM_ print testSudoku2
    putStrLn "-------------\n"

    putStrLn "solving testBoard3:\n-------------"
    mapM_ print testSudoku3
    putStrLn "-------------\n"

    putStrLn "solving testBoard4:\n-------------"
    mapM_ print testSudoku4
    putStrLn "-------------\n"

    --At present, it is unable to solve this board in any reasonable amount of
    --time
    --putStrLn "solving testBoard5:\n-------------"
    --mapM_ print testSudoku5
    --putStrLn "-------------\n"

--rows columns board
testBoard1 = (2, 3,
    ["--36--",
     "-2---4",
     "5---6-",
     "-3---5",
     "3---1-",
     "--14--"])

testBoard2 = (2, 3,
    ["--36--",
     "-2---4",
     "5---6-",
     "-3---5",
     "3---1-",
     "--14--"])

--Har 28 løsninger
testBoard3 = (2, 3,
     ["--1--3",
      "------",
      "----2-",
      "26----",
      "---3--",
      "3--1-2"])

testBoard4 = (3, 3,
    ["134---78-",
     "57-----34",
     "--93---5-",
     "-1-47-6--",
     "84---2---",
     "-9--1---2",
     "-2-----67",
     "7--9-----",
     "-6-7--84-"])

testBoard5 = (4, 4,
     ["---------------5",
      "-A---BF--3E--8-7",
      "--F--A3--82----6",
      "---------------F",
      "----------B-----",
      "-5C--9-------27-",
      "-FB---5------E4-",
      "--------7586----",
      "----E4----------",
      "----2F1--9------",
      "----------6-----",
      "E6D3--------F5-4",
      "---A----3-------",
      "---71C2A8----F--",
      "---D----FB----C-",
      "---1----6-------"])

testSudoku1 = runST $ solveSudoku =<< makeMSudoku rows columns board
    where (rows, columns, board) = testBoard1

testSudoku2 = runST $ solveSudoku =<< makeMSudoku rows columns board
    where (rows, columns, board) = testBoard2

testSudoku3 = runST $ solveSudoku =<< makeMSudoku rows columns board
    where (rows, columns, board) = testBoard3

testSudoku4 = runST $ solveSudoku =<< makeMSudoku rows columns board
    where (rows, columns, board) = testBoard4

testSudoku5 = runST $ solveSudoku =<< makeMSudoku rows columns board
    where (rows, columns, board) = testBoard5
