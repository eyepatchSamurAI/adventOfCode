module Main where

import Control.Exception
import System.IO
import Text.Read (Lexeme (String))
import Text.Printf (printf)
import qualified Data.Set as Set


getDrawnNumbers = take 1


split _ "" = []
split delimiter str =
  let (start, rest) = break (== delimiter) str
      (_, remain) = span (== delimiter) rest
   in start : split delimiter remain

generateBoardSet board = createBoard board
  where
    createBoard [] = []
    createBoard (row:remainingBoard)  =
      (words row) : createBoard remainingBoard

counter num = foldl (+) 0 [0..num] 

isBoardWinner board = (foldl isRowWinner False board) && foldl (isColWinner board) False [0..5]
  where
    isRowWinner [] = False
    isRowWinner (x:xs) = x == '*' || isRowWinner xs
    isColWinner board colNum = foldr (\x acc -> (board !! colNum) == '*' || acc) False board


chunkList :: [a] -> [[a]]
chunkList [] = []
chunkList xs =
  let (chunk, rest) = splitAt 5 xs
  in chunk : chunkList rest

solveChallengePart1 fileName = do
  result <- readFile fileName
  let rawData = lines result
  let drawnNumbers = split ',' (head (getDrawnNumbers rawData))
  let allBoardsRaw = chunkList $ drop 2 rawData 
  let allBoards = map generateBoardSet allBoardsRaw

  print $ (head allBoards)

-- Part 1
main :: IO ()
main = do
  solveChallengePart1 "../data/year2021/challenge4/puzzle.txt"
