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
import Prelude

data Args = Args {
    colors :: Int,
    limit :: Double,
    path :: String
}  deriving (Show, Eq)

options :: [OptDescr (Args -> Args)]
options =
  [ Option "n" ["num-colors"](ReqArg (\n opts -> opts {colors=read n}) "N") "n"
  , Option "l" ["limit"] (ReqArg (\l opts -> opts { limit = read l }) "L") "c"
  , Option "f" ["file-path"] (ReqArg (\f opts -> opts { path = f }) "F") "p"
  ]

type PixelData = ((Int, Int), (Int, Int, Int))

getPixelDataLines :: String -> [PixelData]
getPixelDataLines str = map getPixelData (lines str)

getPixelData :: String -> PixelData
getPixelData str = (getPosition (take 5 str), getColor (drop 6 str))


getPosition :: String -> (Int, Int)
getPosition str = read str :: (Int, Int)

getColor :: String -> (Int, Int, Int)
getColor str = read str :: (Int, Int, Int)


readPixelDataFile :: FilePath -> IO String
readPixelDataFile file_path = readFile file_path

main :: IO ()
main = do
  (opts, args, errors) <- getOpt Permute options <$> getArgs
  unless (null errors) $ do
    exitFailure
  let ic = foldl (flip id) (Args 0 0.0 "") opts
  putStrLn $ "Args options: " ++ show ic

  str <- (readPixelDataFile (path ic))
  putStrLn str
  let pixelData = getPixelDataLines str
  print pixelData
