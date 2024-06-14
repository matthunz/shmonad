import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Data.Maybe (fromMaybe)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.Directory (getCurrentDirectory)
import System.Process (readProcess)

run :: (Traversable t) => t (IO (Maybe String)) -> IO ()
run modules = do
  results <- mapConcurrently id modules
  let prompt = concatMap (fromMaybe "") results
  putStrLn prompt

main :: IO ()
main = do
  run [getCurrentDirectoryIO, getTimeIO, getGitBranchIO]

getCurrentDirectoryIO :: IO (Maybe String)
getCurrentDirectoryIO = Just <$> getCurrentDirectory

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
    Right branch -> Just (init branch)