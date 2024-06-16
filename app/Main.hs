module Main where

import Control.Exception (SomeException)
import Control.Exception.Base (try)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import Options.Applicative
import System.Environment.XDG.BaseDir (getUserConfigDir, getUserDataDir)
import System.FilePath ((</>))
import System.Process (cwd, proc, readCreateProcessWithExitCode, readProcessWithExitCode)

data Command = Default | Init | Recompile

newtype Args = Args
  {aRecompile :: Command}

args :: Parser Args
args =
  Args
    <$> ( subparser
            ( command "init" (info (pure Init) (progDesc "Initialize the shell prompt."))
                <> command "recompile" (info (pure Recompile) (progDesc "Recompile the configuration."))
            )
            <|> pure Default
        )

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (args <**> helper)
        (fullDesc <> progDesc "Shell prompt")

appName :: String
appName = "shmonad"

run :: Args -> IO ()
run (Args Default) = runPrompt
run (Args Init) = runInit
run (Args Recompile) = runRecompile

runPrompt :: IO ()
runPrompt = do
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

runInit :: IO ()
runInit =
  let s =
        unlines
          [ "#!/bin/zsh",
            "setopt prompt_subst",
            "function mkPrompt() { PROMPT=\"$(shmonad)\" }",
            "typeset -a precmd_functions",
            "precmd_functions=(mkPrompt)"
          ]
   in putStrLn s

runRecompile :: IO ()
runRecompile = do
  dataDir <- getUserDataDir ""
  configDir <- getUserConfigDir ""

  let binPath = dataDir </> appName
  let appConfigDir = configDir </> appName

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
