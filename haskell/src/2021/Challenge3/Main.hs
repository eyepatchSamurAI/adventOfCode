module Main where

import Control.Exception
import System.IO
import Text.Read (Lexeme (String))

-- getFileContents :: String -> IO [String]
-- getFileContents fileName = do
--     content <- readFile fileName
--     return (lines content)

combineStrings :: [String] -> [String]
combineStrings = foldr (zipWith (:)) (repeat [])

solveChallengePart1 fileName = do
  result <- readFile fileName
  let rrseult = combineStrings (lines result)
  print rrseult
  let mostBitArray = map mostBit rrseult
  let leastBitArray = map (1 -) mostBitArray
  print mostBitArray
  print leastBitArray
  let gammaRate = fromBNToInt mostBitArray
  let epsilonRate = fromBNToInt leastBitArray
  print (gammaRate * epsilonRate)

solveChallengePart2 fileName = do
  result <- readFile fileName
  let rrseult = combineStrings (lines result)
  print rrseult
  let mostBitArray = map mostBit rrseult
  let leastBitArray = map (1 -) mostBitArray
  print mostBitArray
  print leastBitArray
  let gammaRate = fromBNToInt mostBitArray
  let epsilonRate = fromBNToInt leastBitArray
  print (gammaRate * epsilonRate)


fromBNToInt bitArray = fst $ foldr (\x (acc, i) -> (acc + (x * (2^i)), i + 1)) (0, 0) bitArray

mostBit bitString = if ones > zeros then 1 else 0
  where
    [zeros, ones] = counter bitString [0, 0]
    counter [] score = score
    counter (x : xs) score
      | x == '0' = counter xs (zipWith (+) score [1, 0])
      | x == '1' = counter xs (zipWith (+) score [0, 1])

-- Part 1
main :: IO ()
main = do
  solveChallengePart2 "./src/2021/Challenge3/challenge.txt"
