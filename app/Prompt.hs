module Prompt
  ( backgroundColor,
    textColor,
    currentDirectoryModule,
    gitBranchModule,
    run,
    textModule,
    Color (..),
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Data.Maybe (fromMaybe)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import GHC.IO.Exception (ExitCode)
import System.Console.ANSI.Codes (Color (..), ColorIntensity (Vivid), ConsoleIntensity (BoldIntensity), ConsoleLayer (Background, Foreground), SGR (Reset, SetColor, SetConsoleIntensity), setSGRCode)
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeFileName)
import System.Process (readProcessWithExitCode)

type Module = [IO (Maybe String)]

run :: [IO (Maybe String)] -> IO ()
run modules = do
  results <- mapConcurrently id modules
  let prompt = concatMap (fromMaybe "") results
  putStrLn prompt

textModule :: String -> Module
textModule s = [pure $ Just s]

textColor :: Color -> Module -> Module
textColor color m =
  textModule (setSGRCode [SetColor Foreground Vivid color])
    ++ m
    ++ textModule (setSGRCode [Reset])

backgroundColor :: Color -> Module -> Module
backgroundColor color m =
  textModule (setSGRCode [SetColor Background Vivid color])
    ++ m
    ++ textModule (setSGRCode [Reset])

currentDirectoryModule :: Module
currentDirectoryModule =
  let f = do
        currentDir <- getCurrentDirectory
        let lastPart = takeFileName currentDir
        return $ Just lastPart
   in [f]

timeModule :: Module
timeModule =
  let f = do
        time <- getCurrentTime
        let timeStr = formatTime defaultTimeLocale "%H:%M:%S" time
        return $ Just timeStr
   in [f]

gitBranchModule :: Module
gitBranchModule =
  let f = do
        result <- try (readProcessWithExitCode "git" ["rev-parse", "--abbrev-ref", "HEAD"] "") :: IO (Either SomeException (ExitCode, String, String))
        return $ case result of
          Left _ -> Nothing
          Right (exitCode, branch, _) ->
            if exitCode == ExitSuccess
              then
                if branch == ""
                  then Nothing
                  else Just $ init branch
              else Nothing
   in [f]