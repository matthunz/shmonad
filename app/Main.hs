module Main where

import Control.Exception (SomeException)
import Control.Exception.Base (try)
import Control.Monad (when)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import Options.Applicative
import System.Environment.XDG.BaseDir (getUserConfigDir, getUserDataDir)
import System.FilePath ((</>))
import System.Process (cwd, proc, readCreateProcessWithExitCode, readProcessWithExitCode)

newtype Args = Args
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
run (Args True) = recompile
run (Args False) = prompt

prompt :: IO ()
prompt = do
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
  let appConfigDir = configDir </> "prompt"

  result <-
    try
      ( readCreateProcessWithExitCode
          (proc "stack" ["ghc", "--", "-o", binPath, appConfigDir </> "config.hs"])
            { cwd = Just appConfigDir
            }
          ""
      ) ::
      IO (Either SomeException (ExitCode, String, String))

  case result of
    Left e -> print e
    Right (exitCode, _, stderr) -> do
      if exitCode == ExitSuccess
        then return ()
        else putStrLn stderr
