{-# LANGUAGE OverloadedStrings #-}
module Network.NTP.Control.Packet
  ( Op(..)
  , Variable(..)
  , Variables(..)
  , LeapIndicator(..)
  , ClockSource(..)
  , EventCode(..)
  , ErrorCode(..)
  , Status(..)
  , Packet(..)
  , emptyPacket
  , opVariables
  , readVariables
  ) where

import Control.Monad (when, replicateM_, foldM, liftM)
import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import Data.Char (isSpace)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Lex.Integral (readHexadecimal)
import Data.Fixed (Pico)
import Data.Serialize (Serialize(..), getByteString, putWord8, getWord16be, putWord16be, putByteString)
import Data.Time.Clock (DiffTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word8, Word16)

-- | Available NTP control commands
data Op =
   UnspecifiedOp
 | ReadStatus
 | ReadVariables
 | WriteVariables
 | ReadClockVariables
 | WriteClockVariables
 | SetTrapAddress
 | AsynchronousMessage
 | RuntimeConfiguration
 | SaveConfig
 | UnsetTrap
  deriving (Eq, Show)

decodeTime :: Monad m => BS.ByteString -> m DiffTime
decodeTime b = case (reads :: ReadS Pico) (BSC.unpack b) of
  [(d,"")] -> return $ realToFrac (d / 1000)
  _        -> fail $ "decodeTime: " ++ show b

decodeTimeStamp :: Monad m => BS.ByteString -> m POSIXTime
decodeTimeStamp s | "0x" `BS.isPrefixOf` s = do
  t <- case BSC.split '.' (BS.drop 2 s) of
    [i]   -> fromIntegral `liftM` readHex i
    [i,f] -> do
      i' <- readHex i
      f' <- readHex f
      return $ fromIntegral i' + fromIntegral f' / 16 ^ BS.length f
    _ -> fail $ "decodeTimeStamp: " ++ show s
  return $ t - 0x83aa7e80 -- seconds between 1900 and 1970
decodeTimeStamp s = fail $ "decodeTimeStamp: " ++ show s

readHex :: Monad m => BS.ByteString -> m Integer
readHex s = case readHexadecimal s of
              Just (a,"") -> return a
              _           -> fail $ "readHex: " ++ show s


-- | Currently supported NTP system variables
data Variable =
   Clock
 | RootDispersion
  deriving Show

-- | Record with system variables from a response
data Variables = Variables
  { clock          :: Maybe POSIXTime
  , rootDispersion :: Maybe DiffTime
  }
  deriving Show

encodeVariable :: Variable -> BS.ByteString
encodeVariable Clock          = "clock"
encodeVariable RootDispersion = "rootdisp"

decodeVariable :: Monad m => BS.ByteString -> m Variable
decodeVariable "clock"    = return Clock
decodeVariable "rootdisp" = return RootDispersion
decodeVariable a          = fail $ "decodeVariable: " ++ BSC.unpack a

-- | Construct a packet that for querying an NTP daemon's variables
opVariables :: [Variable] -> Packet
opVariables vs = emptyPacket{ op = ReadVariables
                            , data_ = BS.intercalate "," (map encodeVariable vs)
                            }

-- | Extract variable values from q response packet from an 'opVariables' request
readVariables :: Monad m => Packet -> m Variables
readVariables p = do
  let assign vs [vn,a] = do
        v <- decodeVariable vn
        case v of
          Clock -> do
            ts <- decodeTimeStamp a
            return vs{ clock = Just ts}
          RootDispersion -> do
            t <- decodeTime a
            return vs{ rootDispersion = Just t}
      assign _ s = fail $ "readVariables: " ++ show s
  foldM assign (Variables Nothing Nothing) $
    map (BSC.split '=') $
    BSC.split ',' $
    BSC.filter (not . isSpace) $
    data_ p

encodeOp :: Num a => Op -> a
encodeOp UnspecifiedOp        = 0
encodeOp ReadStatus           = 1
encodeOp ReadVariables        = 2
encodeOp WriteVariables       = 3
encodeOp ReadClockVariables   = 4
encodeOp WriteClockVariables  = 5
encodeOp SetTrapAddress       = 6
encodeOp AsynchronousMessage  = 7
encodeOp RuntimeConfiguration = 8
encodeOp SaveConfig           = 9
encodeOp UnsetTrap            = 31

decodeOp :: (Num a, Eq a, Show a, Monad m) => a -> m Op
decodeOp 0  = return UnspecifiedOp
decodeOp 1  = return ReadStatus
decodeOp 2  = return ReadVariables
decodeOp 3  = return WriteVariables
decodeOp 4  = return ReadClockVariables
decodeOp 5  = return WriteClockVariables
decodeOp 6  = return SetTrapAddress
decodeOp 7  = return AsynchronousMessage
decodeOp 8  = return RuntimeConfiguration
decodeOp 9  = return SaveConfig
decodeOp 31 = return UnsetTrap
decodeOp a  = fail $ "decodeOp: " ++ show a

data LeapIndicator =
   NoWarning
 | Has61Seconds
 | Has59Seconds
 | AlarmCondition
  deriving Show

decodeLeapIndicator :: (Num a, Eq a, Show a, Monad m) => a -> m LeapIndicator
decodeLeapIndicator 0 = return NoWarning
decodeLeapIndicator 1 = return Has61Seconds
decodeLeapIndicator 2 = return Has59Seconds
decodeLeapIndicator 3 = return AlarmCondition
decodeLeapIndicator a = fail $ "decodeLeapIndicator: " ++ show a

data ClockSource =
   UnspecifiedClockSource
 | CalibratedAtomicClock
 | LFRadio
 | HFRadio
 | UHFRadio
 | Local
 | Ntp
 | OtherClockSource
 | WristWatch
 | Telephone
  deriving Show

decodeClockSource :: (Num a, Eq a, Show a, Monad m) => a -> m ClockSource
decodeClockSource 0 = return UnspecifiedClockSource
decodeClockSource 1 = return CalibratedAtomicClock
decodeClockSource 2 = return LFRadio
decodeClockSource 3 = return HFRadio
decodeClockSource 4 = return UHFRadio
decodeClockSource 5 = return Local
decodeClockSource 6 = return Ntp
decodeClockSource 7 = return OtherClockSource
decodeClockSource 8 = return WristWatch
decodeClockSource 9 = return Telephone
decodeClockSource a = fail $ "decodeClockSource: " ++ show a

data EventCode =
   UnspecifiedEventCode
 | FrequencyNotSet
 | FrequencySet
 | SpikeDetect
 | FrequencyMode
 | ClockSync
 | Restart
 | PanicStop
 | NoSysPeer
 | LeapArmed
 | LeapDisarmed
 | LeapEvent
 | ClockStep
 | KernelEvent
 | TAI
 | StaleLeapsecondValues
 | Clockhop
  deriving Show

decodeEventCode :: (Num a, Eq a, Show a, Monad m) => a -> m EventCode
decodeEventCode 0  = return UnspecifiedEventCode
decodeEventCode 1  = return FrequencyNotSet
decodeEventCode 2  = return FrequencySet
decodeEventCode 3  = return SpikeDetect
decodeEventCode 4  = return FrequencyMode
decodeEventCode 5  = return ClockSync
decodeEventCode 6  = return Restart
decodeEventCode 7  = return PanicStop
decodeEventCode 8  = return NoSysPeer
decodeEventCode 9  = return LeapArmed
decodeEventCode 10 = return LeapDisarmed
decodeEventCode 11 = return LeapEvent
decodeEventCode 12 = return ClockStep
decodeEventCode 13 = return KernelEvent
decodeEventCode 14 = return TAI
decodeEventCode 15 = return StaleLeapsecondValues
decodeEventCode 16 = return Clockhop
decodeEventCode a  = fail $ "decodeEventCode: " ++ show a


data ErrorCode =
   UnspecifiedErrorCode
 | AuthenticationFailure
 | BadFormat
 | InvalidOpcode
 | UnknownAssociationID
 | UnknownVariable
 | InvalidVariableValue
 | AdministrativelyProhibited
  deriving Show

decodeErrorCode :: (Num a, Eq a, Show a, Monad m) => a -> m ErrorCode
decodeErrorCode 0 = return UnspecifiedErrorCode
decodeErrorCode 1 = return AuthenticationFailure
decodeErrorCode 2 = return BadFormat
decodeErrorCode 3 = return InvalidOpcode
decodeErrorCode 4 = return UnknownAssociationID
decodeErrorCode 5 = return UnknownVariable
decodeErrorCode 6 = return InvalidVariableValue
decodeErrorCode 7 = return AdministrativelyProhibited
decodeErrorCode a = fail $ "decodeErrorCode: " ++ show a

data Status =
   System LeapIndicator ClockSource Word8 EventCode
 | Error ErrorCode
 | OtherStatus Word16
  deriving Show

toBool :: (Num a, Eq a) => a -> Bool
toBool 0 = False
toBool _ = True

fromBool :: Num a => Bool -> a
fromBool a = if a then 1 else 0

data Packet = Packet
  { responseBit   :: Bool
  , errorBit      :: Bool
  , moreBit       :: Bool
  , op            :: Op
  , sequence      :: Word16
  , status        :: Status
  , associationID :: Word16
  , offset        :: Word16
  , data_         :: BS.ByteString
  }
  deriving Show

emptyPacket :: Packet
emptyPacket = Packet False False False UnspecifiedOp 0 (OtherStatus 0) 0 0 BS.empty

maxDataLength :: Int
maxDataLength = 468

padWord :: Integral a => a -> a
padWord a = (4 - a `mod` 4) `mod` 4

version :: Num a => a
version = 2

mode :: Num a => a
mode = 6 -- NTP control message

instance Serialize Packet where
  put (Packet responseBit' errorBit' moreBit' op' sequence' _status associationID' offset' data_') = do
    putWord16be $  version               `shiftL` 11
               .|. mode                  `shiftL` 8
               .|. fromBool responseBit' `shiftL` 7
               .|. fromBool errorBit'    `shiftL` 6
               .|. fromBool moreBit'     `shiftL` 5
               .|. encodeOp op'
    putWord16be $ sequence'
    putWord16be $ 0
    putWord16be $ associationID'
    putWord16be $ offset'
    putWord16be $ fromIntegral $ BS.length data_'
    putByteString data_'
    replicateM_ (padWord (BS.length data_')) $ putWord8 0

  get = do
    w <- getWord16be
    when (w `shiftR` 11 .&. 0x7 /= version) $ fail "Illegal version"
    when (w `shiftR` 8  .&. 0x7 /= mode) $ fail "Illegal mode"
    let responseBit' = toBool (w `shiftR` 7 .&. 1)
        errorBit'    = toBool (w `shiftR` 6 .&. 1)
        moreBit'     = toBool (w `shiftR` 5 .&. 1)
    op' <- decodeOp (w .&. 0x1f)
    sequence' <- getWord16be
    s <- getWord16be
    associationID' <- getWord16be
    status' <- case (errorBit',op',associationID') of
      (True,_,_) -> Error `fmap` decodeErrorCode (s `shiftR` 8 .&. 0xff)
      (_,_,0) | op' `elem` [ReadStatus, ReadVariables] -> do
        leapIndicator <- decodeLeapIndicator (s `shiftR` 14 .&. 3)
        clockSource   <- decodeClockSource   (s `shiftR` 8  .&. 0x3f)
        let eventCounter = fromIntegral      (s `shiftR` 4  .&. 0xf)
        eventCode      <- decodeEventCode    (s             .&. 0xf)
        return $ System leapIndicator clockSource eventCounter eventCode
      _ -> return $ OtherStatus s
    offset' <- getWord16be
    count <- fromIntegral `fmap` getWord16be
    when (count > maxDataLength) $ fail $ "count too large: " ++ show count
    data_' <- getByteString count
    return $ Packet responseBit' errorBit' moreBit' op' sequence' status' associationID' offset' data_'
