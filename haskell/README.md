#### Running challenges
You can read the adventOfCode.cabal file to see how I seperate each year and challenge. 
You can only run one challenge at a time example: `cabal run 2021-challenge1`


#### Template
This is my template when starting up a new challenge
1. Create new folder under the year and challenge number
2. Add the data to the data directory
3. In my new Mian.hs file use this

```
module Main where


solveChallengePart1 = 
    print ""

main :: IO ()
main = do
  solveChallengePart1 "../data/year2023/challenge1/sample.txt"

```



#### Misc
To Get renaming working in VScode 
- File -> Preferences -> Settings -> Search 'haskell rename'
- Enable experimental cross-module renaming