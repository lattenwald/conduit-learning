{-# LANGUAGE OverloadedStrings #-}
import Conduit
import Data.Conduit.Network
import Data.Word8           (toUpper)

main :: IO ()
main = do
  runTCPServer (serverSettings 4000 "*") $ \appData ->
    appSource appData $$ omapCE toUpper =$ appSink appData
