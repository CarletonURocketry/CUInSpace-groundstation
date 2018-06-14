module XBee (
    XBeeResult(..),
    Frame(..),
    xBeeFrame
) where

import Control.Monad (when)
import Data.Bits ((.&.), testBit)
import Data.Serialize.Get (Get, getWord8, getWord16le, isolate, skip)
import Data.Word (Word8)

import Rocket
import Payload

data XBeeResult = GoodFrame Frame -- ^ A valid frame.
                | NotForUs -- ^ A frame intended for another recipient.
                | InvalidSource Word8 -- ^ Not a valid source address.
                | InvalidType Word8 -- ^ Not a valid frame type.
                | ImpossibleSource Word8 -- ^ Source address cannot produce this type of frame.

data Frame = RocketPrimary RocketFrame
           | RocketAuxilliary
           | UAVTelemetry PayloadFrame
           | ContainerTelemetry ContainerFrame

xBeeFrame :: Get XBeeResult
xBeeFrame = do
    initByte <- getWord8
    when (initByte /= 0x52) $ fail "Invalid start byte in XBee frame."
    srcAddr <- getWord8
    case srcAddr of
        0x02 -> do
            destAddr <- getWord8
            frameType <- getWord8
            lenField <- getWord16le
            let totalLen = fromIntegral $ (lenField .&. 0x7F)
            let crcLen = if testBit lenField 15 then 1 else 0
            let contentLen = totalLen - crcLen
            if destAddr /= 0
                then term totalLen NotForUs
                else case frameType of
                    1 -> if srcAddr == 0x02
                        then fmap (GoodFrame . RocketPrimary) (isolate contentLen rocketFrame) >>= term crcLen
                        else term totalLen (ImpossibleSource srcAddr)
                    2 -> if srcAddr == 0x02
                        then term totalLen (GoodFrame RocketAuxilliary)
                        else term totalLen (ImpossibleSource srcAddr)
                    _ -> term totalLen (InvalidType frameType)
        0x11 -> fmap (GoodFrame . UAVTelemetry) payloadFrame >>= term 1
        0x12 -> fmap (GoodFrame . ContainerTelemetry) containerFrame >>= term 1
        _ -> return (InvalidSource srcAddr)
  where term n v = do
            skip n
            finiByte <- getWord8
            when (finiByte /= 0xCC) $ fail "Invalid stop byte in XBee frame"
            return v
