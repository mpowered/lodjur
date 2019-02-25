{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lodjur.Jobs where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
-- import           Data.Text (Text)
-- import           GitHub.Extra                   ( Sha )
import           System.Directory
import           System.Process

jobLimit :: Int -> IO QSem
jobLimit = newQSem

runJobAsync :: QSem -> IO a -> IO (Async a)
runJobAsync s = async . withQSem s

withQSem :: QSem -> IO a -> IO a
withQSem s = bracket_ (waitQSem s) (signalQSem s)

-- Build

-- build :: GitCfg -> Sha -> IO (Either Text Text)
-- build _ _ = return (Right "")

data GitCfg = GitCfg
  { gitCmd      :: FilePath
  , gitRepoUrl  :: String
  , gitMirror   :: FilePath
  }

runGit :: CreateProcess -> IO ()
runGit p = do
  putStrLn $ "GIT: " ++ show (cmdspec p)
  stdout <- readCreateProcess p ""
  putStrLn stdout
  return ()

git :: GitCfg -> [String] -> CreateProcess
git GitCfg{..} = proc gitCmd

withCwd :: FilePath -> CreateProcess -> CreateProcess
withCwd d p = p { cwd = Just d }

mirror :: GitCfg -> IO ()
mirror cfg =
  runGit $
    git cfg ["clone", gitRepoUrl cfg, "--mirror", gitMirror cfg]

update :: GitCfg -> IO ()
update cfg =
  runGit
    $ withCwd (gitMirror cfg)
    $ git cfg ["remote", "update", "--prune"]

mirrorSync :: GitCfg -> IO ()
mirrorSync cfg = do
  exists <- doesPathExist $ gitMirror cfg
  if exists
    then update cfg
    else mirror cfg

checkout :: GitCfg -> String -> FilePath -> IO ()
checkout cfg sha dest = do
  mirrorSync cfg
  runGit
    $ git cfg ["clone", gitMirror cfg, dest]
  runGit
    $ withCwd dest
    $ git cfg ["checkout", "--detach", sha]
