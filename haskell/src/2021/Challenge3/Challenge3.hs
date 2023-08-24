module Main where
import Control.Exception
import System.IO
import Text.Read (Lexeme(String))


getFileContents :: String -> IO [String]
getFileContents fileName = do
    content <- readFile fileName
    return (lines content)

combineStrings :: [String] -> [String]
combineStrings = foldr (zipWith (:)) (repeat [])

-- solveChallengePart1 :: String -> IO Int
solveChallengePart1 :: String -> IO [String]
solveChallengePart1 fileName = combineStrings <$> getFileContents fileName

-- solveChallengePart2 :: String -> IO Int

main :: IO ()
main = do
  result <- solveChallengePart1 "./src/2021/Challenge3/Challenge3Sample.txt"
  print result
