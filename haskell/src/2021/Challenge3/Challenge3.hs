module Main where
import Control.Exception
import System.IO
import Text.Read (Lexeme(String))
import qualified Data.HashMap.Strict as HashMap


getFileContents :: String -> IO [String]
getFileContents filePath = do
  content <- readFile filePath
  return (lines content)

getColumns :: [String] -> [String]
getColumns = foldr (zipWith (:)) (repeat [])
-- solveChallengePart1 :: String -> IO Int
solveChallengePart1 :: String -> IO [String]
solveChallengePart1 fileName = getColumns <$> getFileContents fileName

-- solveChallengePart2 :: String -> IO Int

main :: IO ()
main = do
  result <- solveChallengePart1 "./src/2021/Challenge3/Challenge3Sample.txt"
  print result
