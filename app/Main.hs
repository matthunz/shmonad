import System.Directory (getCurrentDirectory)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Process (readProcess)
import Control.Exception (try, SomeException)

main :: IO ()
main = do
    cwd <- getCurrentDirectory
    time <- getCurrentTime
    gitBranch <- getGitBranch
    let timeStr = formatTime defaultTimeLocale "%H:%M:%S" time
    putStrLn $ formatPrompt cwd gitBranch timeStr

formatPrompt :: FilePath -> Maybe String -> String -> String
formatPrompt cwd gitBranch timeStr =
    "\ESC[34m" ++ cwd ++
    maybe "" (\branch -> " \ESC[32m(" ++ branch ++ ")") gitBranch ++
    " \ESC[33m[" ++ timeStr ++ "]\ESC[0m\n$ "

getGitBranch :: IO (Maybe String)
getGitBranch = do
    result <- try (readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] "") :: IO (Either SomeException String)
    return $ case result of
        Left _ -> Nothing
        Right branch -> Just (init branch)
