{-# LANGUAGE RankNTypes #-}

module Sudoku where

import Prelude hiding (mapM)
import Data.Foldable (toList)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad (forM_, when)
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
        go (rowsN * columnsN) $ concatMap V.toList rows
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

emptyMSudoku :: Int -> Int -> ST s (MSudoku s)
emptyMSudoku rows columns =
    makeMSudoku rows columns $ replicate width $ replicate width '-'
        where width = rows * columns

--rowsN and columnsN represents then number of rows and columns per box
--Assumes the input is correct
makeMSudoku :: Int -> Int -> [String] -> ST s (MSudoku s)
makeMSudoku rowsN columnsN sudokuTable = do
    let width = rowsN * columnsN
    rows <- newSTMatrix width width
    columns <- newSTMatrix width width
    boxes <- newSTMatrix width width
    let flatSudoku = flattenSudoku sudokuTable
    forM_ flatSudoku (\(row, column, number) -> do
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

flattenSudoku :: (Foldable f, Foldable g) => f (g Char) -> [(Row, Column, Number)]
flattenSudoku sudokuTable =
    let flatSudoku' = concat $ zipWith (\row -> map (\(column, c) -> (row, column, c)))
                      [0..] $ map (zip [0..] . toList) $ toList sudokuTable
        flatSudoku = map (\(row, column, c) ->
                case c of
                    '-' -> (row, column, 0)
                    _   -> (row, column, digitToInt c)) flatSudoku'
    in  flatSudoku

solveSudoku :: MSudoku s -> ST s [Sudoku]
solveSudoku sudoku = do
    let MSudoku rowsPerBox columnsPerBox rows columns boxes = sudoku
    let width = rowsPerBox + columnsPerBox
    squares <- squaresToList rows
    undefined

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
    listRef <- newSTRef []
    forM_ [1..width] (\i -> do
        isPossible <- MV.read possible i
        when isPossible $
            do  list <- readSTRef listRef
                writeSTRef listRef (i:list))
    readSTRef listRef

squaresToList :: Matrix (STRef s Square) -> ST s [STRef s Square]
squaresToList rows =
    return $ V.foldr (flip (V.foldr (:))) [] rows

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
unsafeFreezeSTMatrix stv = do
    v <- unsafeFreeze stv
    mapM unsafeFreeze v

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
