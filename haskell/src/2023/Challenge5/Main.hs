-- {-# LANGUAGE ImportQualifiedPost #-}  USed if you want to import like this import System.IO qualified (readFile)  -- It's just syntax sugar for readablity

module Main where

import qualified System.IO as IO (readFile)
import Data.Char (isDigit)
import Debug.Trace (trace)


dropUntilChar string char =  drop 1 (dropWhile (/= char) string)

getNumber :: String -> Int  -- We make our own function here because read needs to had a type defined
getNumber = read

getMappingRanges :: [String] -> [[String]]
getMappingRanges =  foldr mapRanges []
  where
    mapRanges :: String -> [[String]] -> [[String]]
    mapRanges s []
      | null s = [[]]
      | otherwise = [[]]
    mapRanges s (acc:accs)
      | null s = [] : acc : accs
      | isDigit (head s) = (s : acc) : accs
      | otherwise = acc : accs

convertToIntLists :: [[String]] -> [[[Int]]]
convertToIntLists = map (map (map read . words))

getNewSeedMapping :: Int -> [Int] -> Int
getNewSeedMapping seed specificMapping = foldr getDestination 0 specificMapping
  where
    destination = head specificMapping
    source = specificMapping !! 1
    rangeLength = last specificMapping
    getDestination range acc
      | seed >= source && seed <= source+rangeLength = seed - (source - destination)
      | otherwise = seed


applyAllMappingsToSeed :: Int -> [[Int]] -> Int
applyAllMappingsToSeed seed specificMapping = foldr gogo seed specificMapping
  where
    gogo mapRange acc
      | getNewSeedMapping seed mapRange /= seed = getNewSeedMapping seed mapRange
      | otherwise = acc

applyMappingsToAllSeeds :: [Int] -> [[[Int]]] -> [Int]
applyMappingsToAllSeeds seeds allMappings = map finalSeed seeds
  where
    finalSeed :: Int -> Int
    finalSeed seed = foldl applyAllMappingsToSeed seed allMappings


solveChallengePart1 fileName = do
  fileContent <- fmap lines (readFile fileName)  -- same as  lines <$> readFile fileName
  let seeds =  map getNumber (words (dropUntilChar (head fileContent) ':')) -- `` infix notation
  let rest = convertToIntLists (getMappingRanges (drop 2 fileContent))
  let lowestNumber = minimum (applyMappingsToAllSeeds seeds rest)
  print lowestNumber
  print ""


main :: IO ()
main = do
  solveChallengePart2 "../data/year2023/challenge5/puzzle.txt"


------------------------------------------------------ Part 2 (Not done)
getSeedAndRange :: [Int] -> [Int]
getSeedAndRange [] = []
getSeedAndRange (x1:x2:xs) = [x1..x1+x2-1] ++ getSeedAndRange xs

solveChallengePart2 fileName = do
  fileContent <- fmap lines (readFile fileName)  -- same as  lines <$> readFile fileName
  let seeds = getSeedAndRange (map getNumber (words (dropUntilChar (head fileContent) ':'))) -- `` infix notation
  let rest = convertToIntLists (getMappingRanges (drop 2 fileContent))
  let lowestNumber = minimum (applyMappingsToAllSeeds seeds rest)
  print lowestNumber
  print ""