{-# LANGUAGE OverloadedStrings #-}
module DeleteGist where

import qualified GitHub.Data.Name       as N
import qualified GitHub.Endpoints.Gists as GH

import qualified Data.Text    as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
    let gid = "your-gist-id"
    result <- GH.deleteGist (GH.OAuth "your-token") gid
    case result of
        Left err ->   putStrLn $ "Error: " ++ show err
        Right () -> T.putStrLn $ T.concat ["Deleted: ", N.untagName gid]
