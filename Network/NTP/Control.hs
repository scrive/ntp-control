module Network.NTP.Control
  ( sendTo
  , recvFrom
  , query
  , response
  , queryHost
  ) where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.Serialize (decode, encode)
import Network.BSD (getHostByName, hostAddress)
import qualified Network.NTP.Control.Packet as P
import Network.Socket (Socket, SocketType(..), SockAddr(..), Family(..), socket, close, defaultProtocol)
import qualified Network.Socket.ByteString as N

-- | Send an NTP control packet
sendTo :: Socket -> SockAddr -> P.Packet -> IO Int
sendTo s sa p = N.sendTo s (encode p) sa

-- | Receive an NTP control packet
recvFrom :: Socket -> IO (P.Packet, SockAddr)
recvFrom s = do
  (b, sa) <- N.recvFrom s 1024
  case decode b of
    Left e -> fail e
    Right p -> return (p, sa)

-- | Create a socket and send an NTP control message to a server
query :: SockAddr -> P.Packet -> IO Socket
query sa p = do
  s <- socket AF_INET Datagram defaultProtocol
  void $ sendTo s sa p
  return s

-- | Wait for a response for an issued query
response :: SockAddr -> P.Packet -> Socket -> IO P.Packet
response sa p s = do
  let wait = do
        (p', sa') <- recvFrom s
        if sa' == sa && P.sequence p == P.sequence p'
          then return p'
          else wait
  wait

-- | Convenience wrapper for 'query' that looks up a host name,
-- queries its standard NTP port, and waits for a response
queryHost :: String -> P.Packet -> IO P.Packet
queryHost host p = do
  he <- getHostByName host
  let sa = SockAddrInet 123 (hostAddress he)
  bracket (query sa p) close (response sa p)
