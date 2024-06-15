module Main where

import Control.Exception (SomeException)
import Control.Exception.Base (try)
import Control.Monad (when)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import Options.Applicative
import System.Environment.XDG.BaseDir (getUserConfigDir, getUserDataDir)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

data Args = Args
  {aRecompile :: Bool}

args :: Parser Args
args =
  Args
    <$> switch
      ( long "recompile"
          <> short 'r'
          <> help "Whether to recompile"
      )

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (args <**> helper)
        (fullDesc <> progDesc "Shell prompt")

appName :: String
appName = "prompt"

run :: Args -> IO ()
run args = do
  when (aRecompile args) recompile

  dataDir <- getUserDataDir ""
  let binPath = dataDir </> appName

  result <-
    try
      (readProcessWithExitCode binPath [] "") ::
      IO (Either SomeException (ExitCode, String, String))

  case result of
    Left e -> print e
    Right (exitCode, stdout, stderr) -> do
      if exitCode == ExitSuccess
        then putStrLn stdout
        else putStrLn stderr

recompile :: IO ()
recompile = do
  dataDir <- getUserDataDir ""
  configDir <- getUserConfigDir ""

  let binPath = dataDir </> appName

  result <-
    try
      ( readProcessWithExitCode
          "stack"
          [ "ghc",
            "--",
            "-o",
            binPath,
            configDir </> "prompt" </> "config.hs"
          ]
          ""
      ) ::
      IO (Either SomeException (ExitCode, String, String))

  case result of
    Left e -> print e
    Right (exitCode, _, stderr) -> do
      if exitCode == ExitSuccess
        then return ()
        else putStrLn stderr
