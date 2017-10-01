{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import System.Exit
import Control.Exception
import Control.Monad
import Control.Monad.ST
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Sudoku

type Parser = Parsec Void String

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ do
        pname <- getProgName
        putStrLn "Excpected input file"
        putStrLn $ concat ["Usage: ", pname, " <filepath>"]
        exitSuccess
    let filePath = head args
    sudokuText <- onException (readFile filePath) $ do
        putStrLn $ concat ["The file: ", "\"", filePath, "\" does not exist"]
        exitSuccess
    case parse sudokuTextParser filePath sudokuText of
        Left err -> do
            print err
            exitSuccess
        Right (rows, columns, board) ->
            print $ runST $ solveSudoku =<< makeMSudoku rows columns board

sudokuTextParser :: Parser (Int, Int, [String])
sudokuTextParser = do
    rows <- digitToInt <$> satisfy isSudokuDigit
    newline
    columns <- digitToInt <$> satisfy isSudokuDigit
    newline
    let width = rows * columns
    when (width >= 64) $ fail "Sudoku board is to large"
    board' <- nTimes (width - 1) $ nTimes width
        (satisfy (\c -> isSudokuDigit c || c == '-')) <* newline
    --Since the last line might not have a newline character
    lastLine <- nTimes width (satisfy (\c -> isSudokuDigit c || c == '-'))
    let board = board' ++ [lastLine]
    return (rows, columns, board) <?> "sudokuTextParser"

--Applies the parser n times
nTimes :: Int -> Parser a -> Parser [a]
nTimes n p
    | n < 0 = fail "nTimes negative number"
    | n == 0 = return []
    | n > 0 = do v <- p; (v:) <$> nTimes (n - 1) p <?> "nTimes"
