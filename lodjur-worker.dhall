{ cfgLogFile = Some "worker.log"
, cfgWebSocket = "ws://neptune:4000/websocket"
, cfgGit =
   { gitCommand = "git"
   , gitCache = "gitcache"
   , gitWorkRoot = "workdirs"
   , gitDebug = True
   }
, cfgBuild =
   { buildCommand = "nix-build"
   , buildDebug = True
   }
}
