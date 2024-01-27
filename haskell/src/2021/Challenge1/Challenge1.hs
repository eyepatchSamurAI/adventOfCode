module Main where

import Control.Exception
import System.IO

newtype Product = Product Int
  deriving (Show, Eq)

instance Semigroup Product where
  (Product x) <> (Product y) = Product (x * y)

instance Monoid Product where
  mempty = Product 1
  mappend = (<>)

getChallengeContents :: String -> IO [String]
getChallengeContents fileName = do
  fileContents <- readFile fileName
  return (lines fileContents)

pairs :: [a] -> [(a, a)]
pairs xs = zip (init xs) (tail xs)

slidingTriplets :: [a] -> [(a, a, a)]
slidingTriplets (x:y:z:rest) = (x, y, z) : slidingTriplets (y:z:rest)
slidingTriplets _ = []


compareConsecutive :: Ord a => [a] -> [Ordering]
compareConsecutive xs = map (uncurry compare) (pairs xs)

countLT :: [Ordering] -> Int
countLT xs = length( filter (== LT) xs)

solveChallengePart1 :: String -> IO Int
solveChallengePart1 fileName =  do
  fileContents <- getChallengeContents fileName
  let numbers = map read fileContents :: [Int]
  let numberPairs =  pairs numbers
  let comparisonResults = compareConsecutive numbers
  return (countLT comparisonResults)

solveChallengePart2 :: String -> IO Int
solveChallengePart2 fileName = do 
  fileContents <- getChallengeContents fileName
  let numbers = map read fileContents :: [Int]
  let tripletsSum = map (\(x, y, z) -> x + y + z) (slidingTriplets numbers)
  let numberPairs =  pairs tripletsSum
  let comparisonResults = compareConsecutive tripletsSum
  return (countLT comparisonResults)



main :: IO ()
main = do
  result <- solveChallengePart2 "../data/year2021/challenge1/puzzle.txt"  
  print result
