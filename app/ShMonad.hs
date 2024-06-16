{-# LANGUAGE GADTs #-}

module ShMonad
  ( backgroundColor,
    textColor,
    currentDirectoryModule,
    gitBranchModule,
    userModule,
    shmonad,
    Segment (..),
    path,
    segment,
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
import System.Environment (getEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeFileName)
import System.Process (readProcessWithExitCode)

newtype ShMonad a = Prompt {unPrompt :: IO (Maybe a)}

instance Functor ShMonad where
  fmap f (Prompt a) = Prompt (fmap (fmap f) a)

instance Applicative ShMonad where
  pure a = Prompt . pure $ Just a
  (Prompt f) <*> (Prompt a) = Prompt (liftM2 (<*>) f a)

instance Monad ShMonad where
  return = pure
  (Prompt a) >>= f = Prompt $ do
    maybeValue <- a
    case maybeValue of
      Nothing -> return Nothing
      Just value -> unPrompt (f value)

instance (Monoid a) => Semigroup (ShMonad a) where
  (<>) (Prompt a) (Prompt b) =
    Prompt
      ( do
          results <- mapConcurrently id [a, b]
          return (Just (mconcat (map (fromMaybe mempty) results)))
      )

textModule :: String -> ShMonad String
textModule s = Prompt (pure $ Just s)

currentDirectoryModule :: ShMonad String
currentDirectoryModule =
  let f = do
        currentDir <- getCurrentDirectory
        let lastPart = takeFileName currentDir
        return $ Just lastPart
   in Prompt f

gitBranchModule :: ShMonad String
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

userModule :: ShMonad String
userModule = 
  let f = do
        result <- try (getEnv "USER") :: IO (Either SomeException String)
        return $ case result of
          Left _ -> Nothing
          Right name -> Just name
  in Prompt f

shmonad :: ShMonad String -> IO ()
shmonad (Prompt f) = f >>= \s -> putStr (fromMaybe "" s)

textColor :: ColorIntensity -> Color -> ShMonad String -> ShMonad String
textColor intensity color m =
  textModule ("%{" ++ setSGRCode [SetColor Foreground intensity color] ++ "%}")
    <> m
    <> textModule ("%{" ++ setSGRCode [Reset] ++ "%}")

backgroundColor :: ColorIntensity -> Color -> ShMonad String -> ShMonad String
backgroundColor intensity color m =
  textModule ("%{" ++ setSGRCode [SetColor Background intensity color] ++ "%}")
    <> m
    <> textModule ("%{" ++ setSGRCode [Reset] ++ "%}")

timeModule :: ShMonad String
timeModule =
  let f = do
        time <- getCurrentTime
        let timeStr = formatTime defaultTimeLocale "%H:%M:%S" time
        return $ Just timeStr
   in Prompt f

data Segment = Segment ColorIntensity Color (ShMonad String)

segment :: ColorIntensity -> Color -> ShMonad String -> ShMonad [Segment]
segment intensity color prompt = pure [Segment intensity color prompt]

path :: ShMonad [Segment] -> ShMonad String
path p = p >>= \segments -> pathHelper segments True

pathHelper :: [Segment] -> Bool -> ShMonad String
pathHelper ((Segment intensity color prompt) : segments) isFirst =
  let f (Segment nextIntensity nextColor _) = (nextIntensity, nextColor)
      next = fmap f (safeHead segments)
   in pathSegment intensity color isFirst next prompt <> pathHelper segments False
pathHelper _ _ = Prompt (pure Nothing)

pathSegment :: ColorIntensity -> Color -> Bool -> Maybe (ColorIntensity, Color) -> ShMonad String -> ShMonad String
pathSegment intensity color isFirst next m =
  ( if isFirst
      then textColor intensity color (textModule "\xE0B6")
      else Prompt (pure Nothing)
  )
    <> backgroundColor intensity color m
    <> let x = textColor intensity color $ textModule "\xE0B0"
        in case next of
             Just (nextIntensity, nextColor) -> backgroundColor nextIntensity nextColor x
             Nothing -> x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x
