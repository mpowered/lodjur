{-# LANGUAGE OverloadedStrings #-}

module RSpec where

import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           System.Directory
import           System.FilePath
import           System.Process
import           System.Exit

import           RSpec.Results

data RSpecError
  = RSpecError Int
  | RSpecParseFailed

instance Show RSpecError where
  show (RSpecError code) = "RSpecError: rspec exited with code " <> show code
  show RSpecParseFailed  = "RSpecError: failed to parse rspec output"

instance Exception RSpecError where
  displayException (RSpecError code) = "RSpecError: rspec exited with code " <> show code
  displayException RSpecParseFailed  = "RSpecError: failed to parse rspec output"

runRSpec :: CreateProcess -> IO ()
runRSpec p = do
  putStrLn $ "RSPEC: " ++ show (cmdspec p)
  (exitcode, stdout, stderr) <- readCreateProcessWithExitCode p ""
  mapM_ putStrLn [ "> " <> l | l <- lines stdout ]
  mapM_ putStrLn [ ">> " <> l | l <- lines stderr ]
  case exitcode of
    ExitSuccess      -> return ()
    ExitFailure code -> throwIO $ RSpecError code

rspecCmd :: [String] -> CreateProcess
rspecCmd = proc ".lodjur/rspec"

withCwd :: FilePath -> CreateProcess -> CreateProcess
withCwd d p = p { cwd = Just d }

rspec :: FilePath -> String -> IO RSpecResult
rspec d app = do
  createDirectoryIfMissing True (d </> checkOutput)
  runRSpec $ withCwd d $ rspecCmd [app, checkOutput]
  parseCheckResults (d </> checkOutput)
  where
    checkOutput = "lodjur-check"

parseCheckResults :: FilePath -> IO RSpecResult
parseCheckResults dir = do
  direntries <- listDirectory dir
  let related = filter ("json" `isExtensionOf`) direntries
  files <- filterM doesFileExist $ map (dir </>) related
  mconcat <$> mapM parseCheckResult files

parseCheckResult :: FilePath -> IO RSpecResult
parseCheckResult file = do
  r <- decodeFileStrict' file
  case r of
    Just result -> return result
    Nothing     -> throwIO RSpecParseFailed
