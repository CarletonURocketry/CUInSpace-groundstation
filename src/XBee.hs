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
    destAddr <- getWord8
    frameType <- getWord8
    lenField <- getWord16le
    let totalLen = fromIntegral $ (lenField .&. 0x7F)
    let crcLen = if testBit lenField 15 then 1 else 0
    let contentLen = totalLen - crcLen
    if destAddr /= 0
        then term totalLen NotForUs
        else if srcAddr == 1 || srcAddr > 4
            then term totalLen (InvalidSource srcAddr)
            else case frameType of
                1 -> if srcAddr == 2
                    then fmap (GoodFrame . RocketPrimary) (isolate contentLen rocketFrame) >>= term crcLen
                    else term totalLen (ImpossibleSource srcAddr)
                2 -> if srcAddr == 2
                    then term totalLen (GoodFrame RocketAuxilliary)
                    else term totalLen (ImpossibleSource srcAddr)
                3 -> if srcAddr == 3
                    then fmap (GoodFrame . UAVTelemetry) (isolate contentLen payloadFrame) >>= term crcLen
                    else term totalLen (ImpossibleSource srcAddr)
                4 -> if srcAddr == 4
                    then fmap (GoodFrame . ContainerTelemetry) (isolate contentLen containerFrame) >>= term crcLen
                    else term totalLen (ImpossibleSource srcAddr)
                _ -> term totalLen (InvalidType frameType)
  where term n v = do
            skip n
            finiByte <- getWord8
            when (finiByte /= 0xCC) $ fail "Invalid stop byte in XBee frame"
            return v
