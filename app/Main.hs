module Main where

import Control.Exception (SomeException)
import Control.Exception.Base (try)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import Options.Applicative
import System.Environment.XDG.BaseDir (getUserConfigDir, getUserDataDir)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

data Args = Args
  {sRecompile :: Bool}

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

run :: Args -> IO ()
run (Args True) = recompile
run _ = return ()

recompile :: IO ()
recompile =
  let appName = "prompt"
   in do
        dataDir <- getUserDataDir ""
        configDir <- getUserConfigDir ""

        let binPath = dataDir </> appName

        result <-
          try
            ( readProcessWithExitCode
                "ghc"
                [ "-o",
                  binPath,
                  configDir </> "prompt" </> "prompt.hs"
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
