
module Main where
import System.IO
import Data.Char (isAlphaNum)

isSpecialSymbol c = not (isAlphaNum c) && not ((==) c '.')
solveChallengePart1 fileName = do
    rawFile <- readFile fileName
    let fileLines = lines rawFile
    print ""



main :: IO ()
main = do
  solveChallengePart1 "../data/year2023/challenge3/sample.txt"