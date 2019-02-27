module Main where

import Lodjur.Git

main :: IO ()
main = do
  let env = Env { gitCmd = "git"
                , gitCache = "gitcache"
                , gitWorkRoot = "."
                , gitDebug = True
                }
      repo = Repo "bflyblue" "test"
  workdir <- checkout env repo "1aaed444ff2d1f1165f76e2f44861769c8a52d7d"
  putStrLn workdir
