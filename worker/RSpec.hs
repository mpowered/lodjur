{-# LANGUAGE OverloadedStrings #-}

module RSpec where

import           Control.Exception
import           System.Process
import           System.Exit

newtype RSpecError = RSpecError Int

instance Show RSpecError where
  show (RSpecError code) = "RSpecError: rspec exited with code " <> show code

instance Exception RSpecError where
  displayException (RSpecError code) = "RSpecError: rspec exited with code " <> show code

runRSpec :: CreateProcess -> IO ()
runRSpec p = do
  putStrLn $ "RSPEC: " ++ show (cmdspec p)
  (exitcode, stdout, stderr) <- readCreateProcessWithExitCode p ""
  mapM_ putStrLn [ "> " <> l | l <- lines stdout ]
  mapM_ putStrLn [ ">> " <> l | l <- lines stderr ]
  case exitcode of
    ExitSuccess -> return ()
    ExitFailure code -> throwIO $ RSpecError code

rspecCmd :: [String] -> CreateProcess
rspecCmd = proc ".lodjur/rspec"

withCwd :: FilePath -> CreateProcess -> CreateProcess
withCwd d p = p { cwd = Just d }

rspec :: FilePath -> [String] -> IO ()
rspec d args =
  runRSpec
    $ withCwd d
    $ rspecCmd args
