getChallengeContents :: String -> IO [String]
getChallengeContents fileName = do
  fileContents <- readFile fileName
  return (lines fileContents)

solveChallengePart1 :: String -> IO Int
solveChallengePart1 fileName = do
    fileContents <- getChallengeContents fileName
    return 1

solveChallengePart2 :: String -> IO Int
solveChallengePart2 fileName = do
    fileContents <- getChallengeContents fileName
    return 1

main :: IO ()
main = do
  result <- solveChallengePart1 "../data/year2024/challenge1/sample.txt"
  print result
