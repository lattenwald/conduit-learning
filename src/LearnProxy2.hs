{-# LANGUAGE OverloadedStrings #-}
import Conduit
import Data.Conduit.Network
import Control.Monad            (void)
import Control.Concurrent.Async (concurrently)
import Data.ByteString          (ByteString)
import Data.Word8               (_cr)

creds :: [(ByteString, ByteString)]
creds = [("user", "qwerty")]

checkAuth :: Conduit ByteString IO ByteString
checkAuth = do
  yield "Username: "
  username <- lineAsciiC $ takeCE 80 =$= filterCE (/= _cr) =$= foldC
  yield "Password: "
  password <- lineAsciiC $ takeCE 80 =$= filterCE (/= _cr) =$= foldC
  if (username, password) `elem` creds
     then yield "Successfully authenticated\n"
     else do
       yield "Invalid username/password\n"
       error "Invalid authentication, please log somewhere..."

main :: IO ()
main =
  runTCPServer (serverSettings 4003 "*") $ \client -> do
    (fromClient, ()) <- appSource client $$+ checkAuth =$ appSink client

    runTCPClient (clientSettings 4000 "127.0.0.1") $ \server -> void $ concurrently
        (appSource server $$ appSink client)
        (fromClient $$+- appSink server)
