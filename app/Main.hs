{-
-- EPITECH PROJECT, 2023
-- imageCompressor.hs
-- File description:
-- imageCompressor
-}

import System.Exit
import System.Console.GetOpt
import System.Environment
import Control.Monad
import Data.Maybe
import Data.List
import System.Random (randomRIO)

data Args = Args {
    colors :: Int,
    limit :: Double,
    path :: String
}  deriving (Show, Eq)

type PixelData = ((Int, Int), (Int, Int, Int))

options :: [OptDescr (Args -> Args)]
options =
  [ Option "n" ["num-colors"](ReqArg (\n opts -> opts {colors=read n}) "N") "n"
  , Option "l" ["limit"] (ReqArg (\l opts -> opts { limit = read l }) "L") "c"
  , Option "f" ["file-path"] (ReqArg (\f opts -> opts { path = f }) "F") "p"
  ]

getPixelDataLines :: String -> [PixelData]
getPixelDataLines str = map getPixelData (lines str)

getPixelData :: String -> PixelData
getPixelData str =(getPosition (head (words str)), getColor (last (words str)))

getPosition :: String -> (Int, Int)
getPosition str = read str :: (Int, Int)

getColor :: String -> (Int, Int, Int)
getColor str = read str :: (Int, Int, Int)

readPixelDataFile :: FilePath -> IO String
readPixelDataFile file_path = readFile file_path

getColorList :: [PixelData] -> [(Int, Int, Int)]
getColorList pixelData = map snd pixelData

getRandomCentroids :: [(Int, Int, Int)] -> Int -> IO [(Int, Int, Int)]
getRandomCentroids colorList 0 = return []
getRandomCentroids colorList n = do
  index <- randomRIO (0, length colorList - 1)
  let centroid = colorList !! index
  rest <- getRandomCentroids colorList (n - 1)
  return (centroid : rest)

assignPointsToCentroids :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [Int]
assignPointsToCentroids [] _ = []
assignPointsToCentroids [x] centroids = [getClosestCentroidIndex centroids x]
assignPointsToCentroids (x:xs) centroids =
  getClosestCentroidIndex centroids x : assignPointsToCentroids xs centroids

getClosestCentroidIndex :: [(Int, Int, Int)] -> (Int, Int, Int) -> Int
getClosestCentroidIndex centroids point =
  fromJust $ elemIndex (getClosestCentroid centroids point) centroids

getClosestCentroid :: [(Int, Int, Int)] -> (Int, Int, Int) -> (Int, Int, Int)
getClosestCentroid centroids point =
  minimumBy (\a b -> compare (distance a point) (distance b point)) centroids

distance :: (Int, Int, Int) -> (Int, Int, Int) -> Double
distance (r1, g1, b1) (r2, g2, b2) =
  sqrt $ fromIntegral ((r1 - r2)^2 + (g1 - g2)^2 + (b1 - b2)^2)

updateCentroids :: [(Int, Int, Int)] -> [Int] ->
  [(Int, Int, Int)] -> Int -> [(Int, Int, Int)]
updateCentroids [] _ _ _ = []
updateCentroids [_] indexes colorList i =
  [(averageR points, averageG points, averageB points)]
  where points = getPointsFromCentroid colorList indexes i
updateCentroids (_:xs) indexes colorList i =
  (averageR points, averageG points, averageB points)
  : updateCentroids xs indexes colorList (i + 1)
  where points = getPointsFromCentroid colorList indexes i

getPointsFromCentroid :: [(Int, Int, Int)] -> [Int] -> Int -> [(Int, Int, Int)]
getPointsFromCentroid [] _ _ = []
getPointsFromCentroid [x] indexes i = if (head indexes) == i then [x] else []
getPointsFromCentroid (x:xs) indexes i =
  if (head indexes) == i then
    x : getPointsFromCentroid xs (tail indexes) i
  else getPointsFromCentroid xs (tail indexes) i

averageR :: [(Int, Int, Int)] -> Int
averageR [] = 0
averageR colorList =
  div (sum (map (\(r, _, _) -> r) colorList)) (length colorList)

averageG :: [(Int, Int, Int)] -> Int
averageG [] = 0
averageG colorList =
  div (sum (map (\(_, g, _) -> g) colorList)) (length colorList)

averageB :: [(Int, Int, Int)] -> Int
averageB [] = 0
averageB colorList =
  div (sum (map (\(_, _, b) -> b) colorList)) (length colorList)

kMeans :: [(Int, Int, Int)] -> [(Int, Int, Int)] ->
  [Int] -> Double -> [(Int, Int, Int)]
kMeans centroids colorList indexes limit =
  let updatedCentroids = updateCentroids centroids indexes colorList 0
      converge = isGood (distanceCentroids centroids updatedCentroids limit)
  in if converge
       then updatedCentroids
    else kMeans updatedCentroids colorList
    (assignPointsToCentroids colorList updatedCentroids) limit

isGood :: [Bool] -> Bool
isGood [] = True
isGood [x] = x
isGood (x:xs) = x && isGood xs

distanceCentroids :: [(Int, Int, Int)] -> [(Int, Int, Int)] ->
  Double -> [Bool]
distanceCentroids [] _ _ = [True]
distanceCentroids _ [] _ = [True]
distanceCentroids [x] [y] limit = [distance x y < limit]
distanceCentroids (x:xs) (y:ys) limit = (distance x y < limit)
  : (distanceCentroids xs ys limit)


finalPrint :: [(Int, Int, Int)] -> [Int] -> [PixelData] -> Int -> IO ()
finalPrint [] _  _ _ = return ()
finalPrint _ []  _ _ = return ()
finalPrint _ _  [] _ = return ()
finalPrint [x] indexes colorList i =
  putStrLn "--"
  >> print x
  >> putStrLn "-"
  >> printColorsIndexes indexes colorList i 0
finalPrint (x:xs) indexes colorList i =
  putStrLn "--"
  >> print x
  >> putStrLn "-"
  >> printColorsIndexes indexes colorList i 0
  >> finalPrint xs indexes colorList (i + 1)

printColorsIndexes :: [Int] -> [PixelData] -> Int -> Int -> IO ()
printColorsIndexes [] _ _ _ = return ()
printColorsIndexes [x] colorList i j = if x == i then
  putStr (show (fst (colorList !! j)))
  >> putStr " "
  >> print (snd (colorList !! j))
  else return ()
printColorsIndexes (x:xs) colorList i j = if x == i then
  putStr (show (fst (colorList !! j)))
  >> putStr " "
  >> print (snd (colorList !! j))
  >> printColorsIndexes xs colorList i (j + 1)
  else printColorsIndexes xs colorList i (j + 1)

checkColor :: (Int, Int, Int) -> IO ()
checkColor (r, g, b) =
  if r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255
  then exitWith $ ExitFailure 84
  else return ()

checkColors :: [(Int, Int, Int)] -> IO ()
checkColors [] = return ()
checkColors [x] = checkColor x
checkColors (x:xs) = checkColor x >> checkColors xs

checkArgs :: Args -> IO ()
checkArgs (Args colors limit path) =
  if colors < 1 || limit < 0 || path == ""
  then exitWith $ ExitFailure 84
  else return ()

mainProgram :: Args -> [PixelData] -> [(Int, Int, Int)] -> IO ()
mainProgram ic pixelData colorList = do
  checkColors colorList
  centroids <- getRandomCentroids colorList (colors ic)
  let assignedPoints = assignPointsToCentroids colorList centroids
  let finalCentoids = kMeans centroids colorList assignedPoints (limit ic)
  let indexes = assignPointsToCentroids colorList finalCentoids
  finalPrint finalCentoids indexes pixelData 0

main :: IO ()
main = do
  (opts, args, errors) <- getOpt Permute options <$> getArgs
  unless (null errors) $ exitWith $ ExitFailure 84
  checkArgs $ foldl (flip id) (Args 0 0.0 "") opts
  let ic = foldl (flip id) (Args 0 0.0 "") opts
  str <- (readPixelDataFile (path ic))
  let pixelData = getPixelDataLines str
  let colorList = getColorList pixelData
  mainProgram ic pixelData colorList
