{-# LANGUAGE DeriveAnyClass #-}

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Data.Maybe (fromMaybe)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.Console.ANSI.Codes (Color (Blue), ColorIntensity (Vivid), ConsoleIntensity (BoldIntensity), ConsoleLayer (Foreground), SGR (Reset, SetColor, SetConsoleIntensity), setSGRCode)
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName)
import System.Process (readProcess)

run :: (Traversable t) => t (IO (Maybe String)) -> IO ()
run modules = do
  results <- mapConcurrently id modules
  let prompt = concatMap (fromMaybe "") results
  putStrLn prompt

app :: [IO (Maybe String)]
app =
  currentDirectoryModule
    ++ stringModule " "
    ++ gitBranchModule

main :: IO ()
main = do
  run app

stringModule :: String -> [IO (Maybe String)]
stringModule s = [pure (Just s)]

currentDirectoryModule :: [IO (Maybe String)]
currentDirectoryModule = [getCurrentDirectoryIO]

gitBranchModule :: [IO (Maybe String)]
gitBranchModule = [getGitBranchIO]

getCurrentDirectoryIO :: IO (Maybe String)
getCurrentDirectoryIO = do
  currentDir <- getCurrentDirectory
  let lastPart = takeFileName currentDir
  return $
    Just
      ( setSGRCode [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity]
          ++ lastPart
          ++ setSGRCode [Reset]
      )

getTimeIO :: IO (Maybe String)
getTimeIO = do
  time <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%H:%M:%S" time
  return $ Just timeStr

getGitBranchIO :: IO (Maybe String)
getGitBranchIO = do
  result <- try (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "") :: IO (Either SomeException String)
  return $ case result of
    Left _ -> Nothing
    Right branch ->
      Just
        ( "on "
            ++ setSGRCode [SetColor Foreground Vivid Blue]
            ++ "\xe725 "
            ++ init branch
        )
