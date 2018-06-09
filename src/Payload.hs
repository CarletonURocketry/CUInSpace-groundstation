{-# LANGUAGE RecordWildCards #-}
module Payload (
    ContainerFrame,
    PayloadFrame,
    containerFrame,
    payloadFrame
) where

import Data.Word (Word8, Word16)
import Data.Serialize.Get (Get, getWord8, getWord16le)
import Data.Serialize.IEEE754 (getFloat32le)
import Data.Time.Clock (picosecondsToDiffTime)
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))

import Rocket (GpsData(..))

-- FIXME: what is the endianness of the multibyte quatities in the payload data?
-- Why is the time since midnight measured in milliSiemens?
-- What are the units of mission time?
-- What do vehicle and packet count mean?
-- Order of quaternion components?

data ContainerFrame = ContainerFrame {
    conVehicle :: Word8,
    conPacket :: Word16,
    conGpsData :: GpsData,
    conAltitude :: Float,
    conPressure :: Float,
    conTemperature :: Float,
    conBatteryVoltage :: Float,
    conDeployedByte :: Word8,
    conStateByte :: Word8
}

data PayloadFrame = PayloadFrame {
    paylVehicle :: Word8,
    paylPacket :: Word16,
    paylGpsData :: GpsData,
    paylAltitude :: Float,
    paylPressure :: Float,
    paylTemperature :: Float,
    paylAirspeed :: Float,
    paylAttitude :: Quaternion Float,
    paylBatteryVoltage :: Float,
    paylStateByte :: Word8
}

containerFrame :: Get ContainerFrame
containerFrame = do
    conVehicle <- getWord8
    conPacket <- getWord16le
    conGpsData <- getGpsData
    conAltitude <- getFloat32le
    conPressure <- getFloat32le
    conTemperature <- getFloat32le
    conBatteryVoltage <- getFloat32le
    conDeployedByte <- getWord8
    conStateByte <- getWord8
    return $ ContainerFrame {..}

payloadFrame :: Get PayloadFrame
payloadFrame = do
    paylVehicle <- getWord8
    paylPacket <- getWord16le
    paylGpsData <- getGpsData
    paylAltitude <- getFloat32le
    paylPressure <- getFloat32le
    paylTemperature <- getFloat32le
    paylAirspeed <- getFloat32le
    e <- getFloat32le
    i <- getFloat32le
    j <- getFloat32le
    k <- getFloat32le
    let paylAttitude = Quaternion e (V3 i j k)
    paylBatteryVoltage <- getFloat32le
    paylStateByte <- getWord8
    return $ PayloadFrame {..}

getGpsData :: Get GpsData
getGpsData = do
    utcTime <- fmap (picosecondsToDiffTime . round . (1e6 *)) getFloat32le
    latitude <- fmap realToFrac getFloat32le
    longitude <- fmap fromIntegral getWord16le
    groundSpeed <- fmap fromIntegral getWord16le
    course <- fmap fromIntegral getWord16le
    missionTimeCollected <- fmap round getFloat32le
    return $ GpsData {..}
