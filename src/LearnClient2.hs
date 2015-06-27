{-# LANGUAGE OverloadedStrings #-}
import Conduit
import Data.Conduit.Network
import Control.Concurrent.Async (Concurrently (..))

main :: IO ()
main =
  runTCPClient (clientSettings 4000 "127.0.0.1") $ \server1 ->
  runTCPClient (clientSettings 4001 "127.0.0.1") $ \server2 ->
    runConcurrently $
      Concurrently (stdinC $$ appSink server1) *>
      Concurrently (appSource server1 $$ appSink server2) *>
      Concurrently (appSource server2 $$ stdoutC)
