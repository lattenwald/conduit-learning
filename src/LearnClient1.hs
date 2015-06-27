{-# LANGUAGE OverloadedStrings #-}
import Conduit
import Control.Concurrent.Async (concurrently)
import Control.Monad            (void)
import Data.Conduit.Network

main :: IO ()
main = runTCPClient (clientSettings 4000 "127.0.0.1") $ \server ->
  void $ concurrently
    (stdinC $$ appSink server)
    (appSource server $$ stdoutC)
