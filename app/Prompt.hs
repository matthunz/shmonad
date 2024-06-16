{-# LANGUAGE GADTs #-}

module Prompt
  ( backgroundColor,
    textColor,
    currentDirectoryModule,
    gitBranchModule,
    run,
    textModule,
    Color (..),
    ColorIntensity (..),
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Control.Monad (ap, liftM2)
import Data.Maybe (fromMaybe)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import GHC.IO.Exception (ExitCode)
import System.Console.ANSI.Codes (Color (..), ColorIntensity (..), ConsoleIntensity (BoldIntensity), ConsoleLayer (Background, Foreground), SGR (Reset, SetColor, SetConsoleIntensity), setSGRCode)
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeFileName)
import System.Process (readProcessWithExitCode)

newtype Prompt a = Prompt {unPrompt :: IO (Maybe a)}

instance Functor Prompt where
  fmap f (Prompt a) = Prompt (fmap (fmap f) a)

instance Applicative Prompt where
  pure a = Prompt . pure $ Just a
  (Prompt f) <*> (Prompt a) = Prompt (liftM2 (<*>) f a)

instance Monad Prompt where
  return = pure
  (Prompt a) >>= f = Prompt $ do
    maybeValue <- a
    case maybeValue of
      Nothing -> return Nothing
      Just value -> unPrompt (f value)


instance (Monoid a) => Semigroup (Prompt a) where
  (<>) (Prompt a) (Prompt b) =
    Prompt
      ( do
          results <- mapConcurrently id [a, b]
          return (Just (mconcat (map (fromMaybe mempty) results)))
      )

textModule :: String -> Prompt String
textModule s = Prompt (pure $ Just s)

currentDirectoryModule :: Prompt String
currentDirectoryModule =
  let f = do
        currentDir <- getCurrentDirectory
        let lastPart = takeFileName currentDir
        return $ Just lastPart
   in Prompt f

gitBranchModule :: Prompt String
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
   in Prompt f

run :: Prompt String -> IO ()
run (Prompt f) = f >>= \s -> putStr (fromMaybe "" s)

textColor :: ColorIntensity -> Color -> Prompt String -> Prompt String
textColor intensity color m =
  textModule ("%{" ++ setSGRCode [SetColor Foreground intensity color] ++ "%}")
    <> m
    <> textModule ("%{" ++ setSGRCode [Reset] ++ "%}")

backgroundColor :: ColorIntensity -> Color -> Prompt String -> Prompt String
backgroundColor intensity color m =
  textModule ("%{" ++ setSGRCode [SetColor Background intensity color] ++ "%}")
    <> m
    <> textModule ("%{" ++ setSGRCode [Reset] ++ "%}")

timeModule :: Prompt String
timeModule =
  let f = do
        time <- getCurrentTime
        let timeStr = formatTime defaultTimeLocale "%H:%M:%S" time
        return $ Just timeStr
   in Prompt f
