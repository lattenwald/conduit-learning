{-# LANGUAGE OverloadedStrings #-}
import Conduit
import Data.Conduit.Network
import Control.Monad            (void)
import Control.Concurrent.Async (Concurrently (..))

main :: IO ()
main =
  runTCPServer (serverSettings 4002 "*") $ \appData ->
  runTCPClient (clientSettings 4000 "127.0.0.1") $ \server -> void $
    runConcurrently $
      Concurrently (appSource server $$ appSink appData) *>
      Concurrently (appSource appData $$ appSink server)
