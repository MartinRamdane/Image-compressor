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
getPixelData str = (getPosition (head (words str)), getColor (last (words str)))

getPosition :: String -> (Int, Int)
getPosition str = read str :: (Int, Int)

getColor :: String -> (Int, Int, Int)
getColor str = read str :: (Int, Int, Int)

readPixelDataFile :: FilePath -> IO String
readPixelDataFile file_path = readFile file_path

getColorList :: [PixelData] -> [(Int, Int, Int)]
getColorList pixelData = map snd pixelData

getFirstsCentroids :: [(Int, Int, Int)] -> Int -> [(Int, Int, Int)]
getFirstsCentroids colorList n = take n colorList

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

kMeans :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [Int] -> Double -> [Int]
kMeans centroids colorList indexes limit =
  let updatedCentroids = updateCentroids centroids indexes colorList 0
      distance = calculateDistanceBetweenCentroids centroids updatedCentroids
  in if distance < limit
       then indexes
       else kMeans updatedCentroids colorList (assignPointsToCentroids colorList updatedCentroids) limit

calculateDistanceBetweenCentroids :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> Double
calculateDistanceBetweenCentroids centroids updatedCentroids =
  sum $ map (\(c, uc) -> distance c uc) (zip centroids updatedCentroids)


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


main :: IO ()
main = do
  (opts, args, errors) <- getOpt Permute options <$> getArgs
  unless (null errors) $ exitFailure
  let ic = foldl (flip id) (Args 0 0.0 "") opts
  str <- (readPixelDataFile (path ic))
  let pixelData = getPixelDataLines str
  let colorList = getColorList pixelData
  let centroids = getFirstsCentroids colorList (colors ic)
  let assignedPoints = assignPointsToCentroids colorList centroids
  let finalIndexes = kMeans centroids colorList assignedPoints (limit ic)
  finalPrint centroids finalIndexes pixelData 0
