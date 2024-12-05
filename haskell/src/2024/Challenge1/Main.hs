import Data.Bifunctor (Bifunctor(bimap))
import Data.List (sort)
getChallengeContents :: String -> IO [String]
getChallengeContents fileName = do
  fileContents <- readFile fileName
  return (lines fileContents)


getLists :: Monad m => [String] -> m ([Int], [Int])
getLists contents = do
    let combinedLists = [(x,y) | line  <- contents, let [x,y] = map read (words line) :: [Int]]
    let (list1, list2) = unzip combinedLists
    return (list1, list2)

solveChallengePart1 :: String -> IO Int
solveChallengePart1 fileName = do
    fileContents <- getChallengeContents fileName
    tt <- getLists fileContents
    let (sortedList1, sortedList2) = bimap sort sort tt
    let n = sum (zipWith (\x y -> abs (x - y)) sortedList1 sortedList2)
    return n

main :: IO ()
main = do
  result <- solveChallengePart1 "../data/year2024/challenge1/puzzle.txt"
  print result
