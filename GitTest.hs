module Main where

import Lodjur.Git

main :: IO ()
main = do
  let opts = Options { gitCmd = "git"
                    , gitCache = "gitcache"
                    }
      repo = Repo "bflyblue" "test"
  checkout opts repo "1aaed444ff2d1f1165f76e2f44861769c8a52d7d" "test-1"
