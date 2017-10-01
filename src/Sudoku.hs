{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}

module Sudoku (
MSudoku(..),
Sudoku(..),
Square(..),
emptyMSudoku,
makeMSudoku,
solveSudoku
) where

import Prelude hiding (mapM, ($))
import Data.Foldable (toList)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad (forM_, when, filterM)
import Control.Monad.ST
import Data.Char hiding (intToDigit, digitToInt)
import Data.STRef
import Data.Vector (Vector, mapM, unsafeFreeze, (!))
import Data.Vector.Mutable (MVector, STVector, new, write)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

data MSudoku s = MSudoku { rowsPerBox    :: Int,
                           columnsPerBox :: Int,
                           rows    :: Matrix (STRef s Square),
                           columns :: Matrix (STRef s Square),
                           boxes   :: Matrix (STRef s Square) }

data Sudoku = Sudoku     { rowsPerBox'    :: Int,
                           columnsPerBox' :: Int,
                           rows'    :: Matrix Square,
                           columns' :: Matrix Square,
                           boxes'   :: Matrix Square }

data Square = Square     { row    :: Row,
                           column :: Column,
                           box    :: Box,
                           number :: Number } deriving (Show)

type STMatrix s a = STVector s (STVector s a)
type Matrix a = Vector (Vector a)
type Row = Int
type Column = Int
type Box = Int
type Number = Int

instance Show Sudoku where
    show (Sudoku rowsN columnsN rows _ _) =
        go (rowsN * columnsN) $ matrixToList rows
        where
            go width [] = line width
            go width (Square row column box number : rows)
                | column == 0 && row `mod` rowsN == 0 =
                    line width ++ "| " ++ intToDigit number : " "
                    ++ go width rows
                | column `mod` columnsN ==  0 = "| " ++ intToDigit number : " "
                    ++ go width rows
                | column == width - 1 =
                    intToDigit number : " |\n" ++ go width rows
                | otherwise = intToDigit number : " "
                    ++ go width rows
            line width
                | width == 0 = "+\n"
                | width `mod` columnsN == 0 = "+---" ++ line (width - 1)
                | otherwise = "--" ++ line (width - 1)

--replace standard $ with the strict version
($) = ($!)
infixr 0 $

--Converts any number 0 - 63 to a char value. Any digit above 9 is converted to
--a char starting with 'A'
intToDigit :: Int -> Char
intToDigit n
    | n >= 0 && n < 10 = chr $ n + ord '0'
    | n >= 10 && n < 64 = chr $ (n - 10) + ord 'A'

--Does the reverse of intToDigit
digitToInt :: Char -> Int
digitToInt c
    | isDigit c = ord c - ord '0'
    | c >= 'A' && c < chr (ord 'A' + 54) = ord c - ord 'A' + 10

changeSquareNumber :: Square -> Int -> Square
changeSquareNumber (Square row column box _) = Square row column box
{-# INLINE changeSquareNumber #-}

emptyMSudoku :: Int -> Int -> ST s (MSudoku s)
emptyMSudoku rows columns =
    makeMSudoku rows columns $ replicate width $ replicate width '-'
        where width = rows * columns

--rowsN and columnsN represents the number of rows and columns per box
--Assumes the input is correct
makeMSudoku :: Int -> Int -> [String] -> ST s (MSudoku s)
makeMSudoku rowsN columnsN sudokuTable = do
    let width = rowsN * columnsN
    rows <- newSTMatrix width width
    columns <- newSTMatrix width width
    boxes <- newSTMatrix width width
    let rowColumnList = makeRowColumnList sudokuTable
    forM_ rowColumnList (\(row, column, number) -> do
        let box = (column `div` columnsN) + rowsN * (row `div` rowsN)
        let boxColumn = (column `mod` columnsN) + columnsN * (row `mod` rowsN)
        square <- newSTRef $ Square row column box number
        writeSTMatrix rows row column square
        writeSTMatrix columns column row square
        writeSTMatrix boxes box boxColumn square)
    rows' <- unsafeFreezeSTMatrix rows
    columns' <- unsafeFreezeSTMatrix columns
    boxes' <- unsafeFreezeSTMatrix boxes
    return $ MSudoku rowsN columnsN rows' columns' boxes'

--Makes a list containing the rows, columns and numbers for each Square
makeRowColumnList :: (Foldable f, Foldable g) => f (g Char) -> [(Row, Column, Number)]
makeRowColumnList sudokuTable =
    let rowColumnList' = concat $ zipWith (\row -> map (\(column, c) -> (row, column, c)))
                      [0..] $ map (zip [0..] . toList) $ toList sudokuTable
        rowColumnList = map (\(row, column, c) ->
                case c of
                    '-' -> (row, column, 0)
                    _   -> (row, column, digitToInt c)) rowColumnList'
    in  rowColumnList

solveSudoku :: MSudoku s -> ST s [Sudoku]
solveSudoku sudoku = do
    let MSudoku rowsPerBox columnsPerBox rows columns boxes = sudoku
    let width = rowsPerBox + columnsPerBox
    squares <- filterZeroes =<< squaresToList rows
    solutionList <- newSTRef []
    solveSudoku' sudoku squares solutionList
    readSTRef solutionList

solveSudoku'    :: MSudoku s        --The sudoku board to be solved
                -> [STRef s Square] --List of Squares
                -> STRef s [Sudoku] --List of solutions
                -> ST s ()
--If we have only one element left
solveSudoku' sudoku [square] solutionList = do
    square' <- readSTRef square
    solutions <- possibleSquareValues sudoku square'
    forM_ solutions (\num -> do
        modifySTRef' square $ flip changeSquareNumber num
        solution <- freezeMSudoku sudoku
        modifySTRef' solutionList (solution:)
        modifySTRef' square $ flip changeSquareNumber 0)

--For all other cases
solveSudoku' sudoku (square:squares) solutionList = do
    square' <- readSTRef square
    solutions <- possibleSquareValues sudoku square'
    forM_ solutions (\num -> do
        modifySTRef' square $ flip changeSquareNumber num
        solveSudoku' sudoku squares solutionList
        modifySTRef' square $ flip changeSquareNumber 0)

possibleSquareValues :: MSudoku s -> Square -> ST s [Int]
possibleSquareValues sudoku square = do
    let MSudoku rowsPerBox columnsPerBox rows columns boxes = sudoku
    let Square row column box _ = square
    let width = rowsPerBox * columnsPerBox
    possible <- new $ width + 1
    --init values
    forM_ [0..width] (\i -> MV.write possible i True)
    --Exclude possible numbers
    forM_ (rows ! row) (\square -> do
        Square _ _ _ number <- readSTRef square
        MV.write possible number False)
    forM_ (columns ! column) (\square -> do
        Square _ _ _ number <- readSTRef square
        MV.write possible number False)
    forM_ (boxes ! box) (\square -> do
        Square _ _ _ number <- readSTRef square
        MV.write possible number False)
    --Make and return a list of possible numbers
    list <- newSTRef []
    forM_ [1..width] (\i -> do
        isPossible <- MV.read possible i
        when isPossible $ modifySTRef' list (i:))
    readSTRef list
--Inlining here might be overkill, but the function is only used in solveSudoku
--so it might increase the performance somewhat.
{-# INLINE possibleSquareValues #-}

matrixToList :: Matrix a -> [a]
matrixToList = foldr (flip (foldr (:))) []
{-# INLINE matrixToList #-}

squaresToList :: Matrix (STRef s Square) -> ST s [STRef s Square]
squaresToList m = return $ matrixToList m
{-# INLINE squaresToList #-}

--Creates a list containing only empty squares
filterZeroes :: [STRef s Square] -> ST s [STRef s Square]
filterZeroes squares = do
    list <- newSTRef []
    --This will also reverse the order of the list in addition to filter out
    --non zero values.
    forM_ squares (\square -> do
        Square _ _ _ number <- readSTRef square
        modifySTRef' list (\list -> if number == 0 then square:list else list))
    --I don't actually have to re-reverse the list, but then I know the squares
    --will be evaluated from top to bottom, instead of the opposite.
    modifySTRef' list reverse
    readSTRef list
{-# INLINE filterZeroes #-}

freezeMSudoku :: MSudoku s -> ST s Sudoku
freezeMSudoku (MSudoku rowsPerBox columnsPerBox rows columns boxes) = do
    rows' <- readSTRefMatrix rows
    columns' <- readSTRefMatrix columns
    boxes' <- readSTRefMatrix boxes
    return $ Sudoku rowsPerBox columnsPerBox rows' columns' boxes'

newSTMatrix :: Int -> Int -> ST s (STMatrix s a)
newSTMatrix rows columns =
    MV.replicateM rows (new columns)

unsafeFreezeSTMatrix :: STMatrix s a -> ST s (Matrix a)
unsafeFreezeSTMatrix stm = do
    m <- unsafeFreeze stm
    mapM unsafeFreeze m

replicateSTMatrix :: Int -> Int -> a -> ST s (STMatrix s a)
replicateSTMatrix rows columns x =
    MV.replicateM rows (MV.replicate columns x)

modifySTMatrix :: STMatrix s a -> (a -> a) -> Int -> Int -> ST s ()
modifySTMatrix m f row column = do
    mc <- MV.read m row --matrix column
    MV.modify mc f column

readSTMatrix :: STMatrix s a -> Int -> Int -> ST s a
readSTMatrix m row column = do
    mc <- MV.read m row
    MV.read mc column

writeSTMatrix :: STMatrix s a -> Int -> Int -> a -> ST s ()
writeSTMatrix m row column x = do
    mc <- MV.read m row
    write mc column x

readSTRefMatrix :: Matrix (STRef s a) -> ST s (Matrix a)
readSTRefMatrix = (mapM . mapM) readSTRef
