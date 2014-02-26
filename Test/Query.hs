import Network.NTP.Control (queryHost)
import Network.NTP.Control.Packet (opVariables, Variable(..), readVariables)
import System.Timeout (timeout)

main :: IO ()
main = do
  mres <- timeout 1000000 $ do
    queryHost "localhost" $ opVariables [Clock, RootDispersion]
  case mres of
    Nothing -> fail "Timeout"
    Just p -> do
      print p
      readVariables p >>= print
