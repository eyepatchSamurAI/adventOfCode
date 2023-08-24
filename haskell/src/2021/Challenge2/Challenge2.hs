module Main where

import System.IO ()
import Text.Read (readMaybe)
import Debug.Trace (traceShow )

data DirectionUnit = DirectionUnit {direction :: Direction, value :: Int}

data Direction = Up Int | Down Int | Forward Int deriving (Show, Eq)

split :: Char -> String -> [String]
split _ "" = []
split delimiter str =
  let (start, rest) = break (== delimiter) str
      (_, remain) = span (== delimiter) rest
   in start : split delimiter remain

instance Read Direction where
    readsPrec _ input =
        case lex input of
            [("up", rest)]     -> parseNumber Up rest
            [("down", rest)]   -> parseNumber Down rest
            [("forward", rest)] -> parseNumber Forward rest
            _                  -> []
        where
            parseNumber cons rest =
                case lex rest of
                    [(num, remaining)] -> [(cons (read num), remaining)]
                    _                  -> []


data Coordinates = Coordinates {x :: Int, y :: Int} deriving(Show)

updateCoordinates :: Coordinates -> Direction -> Coordinates
updateCoordinates (Coordinates x y) (Up n) = Coordinates x (y + n)
updateCoordinates (Coordinates x y) (Down n) = Coordinates x (y - n)
updateCoordinates (Coordinates x y) (Forward n) = Coordinates (x + n) y

readFileContents :: String -> IO [String]
readFileContents fileName = do
    content <- readFile fileName
    return (lines content)

move :: [String] -> Coordinates
move content = 
    let directions = map read content :: [Direction]
        initialCoordinates = Coordinates 0 0
    in foldl updateCoordinates initialCoordinates directions

computeResult :: Coordinates -> Int
computeResult coords = x coords * abs (y coords)

-- solveChallengePart1 :: String -> IO Int
-- solveChallengePart1 fileName = do
--     content <- readFileContents fileName
--     let result = move content
--     print result
--     return $ computeResult result

solveChallengePart1 :: String -> IO Int
solveChallengePart1 fileName = 
    computeResult . move <$> readFileContents fileName

--------------------------------
-- Not used
class HasCoordinates a where
    getX :: a -> Int
    getY :: a -> Int

instance HasCoordinates Coordinates where
    getX = x
    getY = y

instance HasCoordinates Coordinates2 where
    getX = hortz
    getY = vert

data EitherCoords = Coords Coordinates | Coords2 Coordinates2
computeCoords :: EitherCoords -> Int
computeCoords coords =
    case coords of
        Coords c  -> x c * abs (y c)
        Coords2 c -> hortz c * abs (vert c)


data Coordinates2 = Coordinates2 {hortz :: Int, vert :: Int, aim :: Int} deriving(Show)

updateCoordinates2 :: Coordinates2 -> Direction -> Coordinates2
updateCoordinates2 coords dir = 
    traceShow updatedCoords updatedCoords
  where
    updatedCoords = case dir of
      Down n     -> Coordinates2 x y (aim + n)
      Up n       -> Coordinates2 x y (aim - n)
      Forward n  -> Coordinates2 (x + n) (y + (aim * n)) aim
    Coordinates2 x y aim = coords

computeCoordinates2 :: [String] -> Coordinates2
computeCoordinates2 content = 
    let directions = map read content :: [Direction]
        initialCoordinates2 = Coordinates2 0 0 0
    in foldl updateCoordinates2 initialCoordinates2 directions

computeResult2 :: Coordinates2 -> Int
computeResult2 coords = hortz coords * abs (vert coords)

solveChallengePart2 :: String -> IO Int
solveChallengePart2 filename = (.) computeResult2 computeCoordinates2 <$> readFileContents filename


main :: IO ()
main = do
  result <- solveChallengePart2 "./src/2021/Challenge2/Challenge2.txt"
  print result