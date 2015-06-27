{-# LANGUAGE OverloadedStrings #-}
import Conduit
import Data.Conduit.Network
import Data.ByteString      (pack)

main :: IO ()
main = runTCPServer (serverSettings 4001 "*") $ \appData ->
  appSource appData
    $$ concatMapCE (\w -> pack [w, w])
    =$ appSink appData
