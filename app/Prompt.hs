module Prompt (run, currentDirectoryModule, gitBranchModule) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Data.Maybe (fromMaybe)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import GHC.IO.Exception (ExitCode)
import System.Console.ANSI.Codes (Color (Blue), ColorIntensity (Vivid), ConsoleIntensity (BoldIntensity), ConsoleLayer (Foreground), SGR (Reset, SetColor, SetConsoleIntensity), setSGRCode)
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeFileName)
import System.Process (readProcessWithExitCode)

run :: [IO (Maybe String)] -> IO ()
run modules = do
  results <- mapConcurrently id modules
  let prompt = unwords (map (fromMaybe "") results)
  putStrLn prompt

currentDirectoryModule :: IO (Maybe String)
currentDirectoryModule = do
  currentDir <- getCurrentDirectory
  let lastPart = takeFileName currentDir
  return $
    Just
      ( setSGRCode [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity]
          ++ lastPart
          ++ setSGRCode [Reset]
      )

timeModule :: IO (Maybe String)
timeModule = do
  time <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%H:%M:%S" time
  return $ Just timeStr

gitBranchModule :: IO (Maybe String)
gitBranchModule = do
  result <- try (readProcessWithExitCode "git" ["rev-parse", "--abbrev-ref", "HEAD"] "") :: IO (Either SomeException (ExitCode, String, String))
  return $ case result of
    Left _ -> Nothing
    Right (exitCode, branch, _) ->
      if exitCode == ExitSuccess
        then
          Just
            ( "on "
                ++ setSGRCode [SetColor Foreground Vivid Blue]
                ++ "\xe725 "
                ++ branch
            )
        else
          Nothing
