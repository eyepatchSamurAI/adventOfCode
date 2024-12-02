module Main where

import Data.Char
import Data.List
import Debug.Trace (trace)
import System.IO

prettyPrintList :: (Show a) => [a] -> String
prettyPrintList lst = unlines $ map show lst

splitAndRemove :: (Eq a) => a -> [a] -> ([a], [a])
splitAndRemove char string = case break (== char) string of
  (before, _ : after) -> (before, after)
  (before, []) -> (before, [])

trim = dropWhileEnd isSpace . dropWhile isSpace

isNumberWinner winners card = map (\number -> number `elem` winners) card

scoreTheCard :: [Bool] -> Int
scoreTheCard boolResults = scorer boolResults 0
  where
    scorer [] score = score
    scorer (x : xs) score =
      if x
        then
          if score >= 1
            then scorer xs score * 2
            else scorer xs 1
        else scorer xs score

getNumbers :: String -> Char -> [String]
getNumbers string delim = wordsWhen (== delim) string
  where
    wordsWhen :: (Char -> Bool) -> String -> [String]
    wordsWhen p s = case dropWhile p s of
      "" -> []
      s' -> w : wordsWhen p s''
        where
          (w, s'') = break p s' --  s'' is the third version (s -> s' -> s'')

solveChallengePart1 fileName = do
  rawFile <- readFile fileName
  let fileLines = lines rawFile
  let winnersAndCardRaw = map (splitAndRemove '|' . snd . splitAndRemove ':') fileLines
  let winnersAndCard = map (\x -> (trim (fst x), trim (snd x))) winnersAndCardRaw
  let parsed = map (\x -> (getNumbers (fst x) ' ', getNumbers (snd x) ' ')) winnersAndCard
  let score = map (\x -> (isNumberWinner (fst x) (snd x))) parsed
  let finalscore = sum $ map (scoreTheCard) score
  print finalscore

-- scoreCard
ones n = replicate n 1 --  [1 | _ <- [1..100]]


------------------------------------------------------------------------------------------------------------------



solveChallengePart2 fileName = do
  fileContent <- readFile fileName
  let intLines = lineChallenge $ lines fileContent
  let numberOfWinners = map (uncurry numberOfMatches) intLines
  let score = tally numberOfWinners (ones (length numberOfWinners))
  -- putStr $ prettyPrintList score
  print score
  print ""

lineChallenge = map (parseIntoNumbers . parseString)

updateAmountMapping current [] = current
updateAmountMapping [] new = new
updateAmountMapping (c : cs) (n : new) = c + n : updateAmountMapping cs new

parseString :: String -> (String, String)
parseString string =
  let (_, rest) = splitAndRemove ':' string
   in splitAndRemove '|' rest

numberOfMatches :: [Int] -> [Int] -> Int
numberOfMatches winner [] = 0
numberOfMatches winner (c : challenges) =
  if c `elem` winner
    then 1 + numberOfMatches winner challenges
    else 0 + numberOfMatches winner challenges

addToFirstN :: Int -> Int -> [Int] -> [Int]
addToFirstN numberOfElems amount xs = map (+ amount) (take numberOfElems xs) ++ drop numberOfElems xs

tally :: [Int] -> [Int] -> Int
tally numberOfWinners [] = 0
tally [] multipliers = 0
tally (x : numberOfWinners) (y : multipliers) =
  -- trace ("numberOfWinners: " ++ show  numberOfWinners) $
  -- trace ("multipliers: " ++ show multipliers) $
  y + tally numberOfWinners (addToFirstN x (1 * y) multipliers)

parseIntoNumbers :: (String, String) -> ([Int], [Int])
parseIntoNumbers (winner, challenge) = (map read . words $ winner, map read . words $ challenge)

main :: IO ()
main = do
  solveChallengePart2 "../data/year2023/challenge4/puzzle.txt"
