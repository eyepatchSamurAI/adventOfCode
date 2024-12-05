import Distribution.Simple.Setup (trueArg)
getChallengeContents :: String -> IO [String]
getChallengeContents fileName = do
  fileContents <- readFile fileName
  return (lines fileContents)


isIncreasing first second = first < second
isDecreasing first second = first > second

-- is_safe [] = true
-- is safe (current, next: rest) = current

chooseIncrementFunction :: Int -> Int -> Maybe (Int -> Int -> Bool)
chooseIncrementFunction firstNum secondNum = 
    case compare secondNum firstNum of
        LT -> Just isDecreasing
        GT -> Just isIncreasing
        EQ -> Nothing


solveChallengePart1 :: String -> IO Int
solveChallengePart1 fileName = do
    fileContents <- getChallengeContents fileName
    let allReports = [x | line <- fileContents, let x = map read (words line) :: [Int]]
    let firstReport = head allReports
    let [firstNum, secondNum] = take 2 firstReport
    let incrementFunction = chooseIncrementFunction firstNum secondNum
    return 1

solveChallengePart2 :: String -> IO Int
solveChallengePart2 fileName = do
    fileContents <- getChallengeContents fileName
    return 1

main :: IO ()
main = do
  result <- solveChallengePart1 "../data/year2024/challenge2/sample.txt"
  print result
