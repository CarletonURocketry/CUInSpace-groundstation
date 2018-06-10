{-# LANGUAGE RecordWildCards #-}
module Rocket (
    RocketFrame,
    GpsData(..),
    State(..),
    rocketFrame
) where

import Data.Bits ((.&.), testBit)
import Data.Int (Int8)
import Data.Serialize.Get (Get, getInt8, getInt16le, getInt32le, getWord16le, getWord32le, skip)
import Data.Text (pack)
import Data.Time (TimeOfDay, defaultTimeLocale, formatTime, picosecondsToDiffTime, timeToTimeOfDay)
import Data.Word (Word32)
import Graphics.QML (DefaultClass(..), defPropertyRO, fromObjRef)

data RocketFrame = RocketFrame {
    missionTime :: Word32, -- ^ Time since initialization at which the packet was sent (ms).
    capacitorVoltage :: Double, -- ^ Potential difference across the capacitor (V).
    batteryVoltage :: Double, -- ^ Potential difference across the battery (V).
    batteryTemp :: Double, -- ^ Temperature of batteries from ADC (°C).
    ambientTemp :: Double, -- ^ Ambient temperature according to ADC (°C).
    gyroTemp :: Int8, -- ^ Temperature according to gyroscope (°C).
    altimeterTemp :: Double, -- ^ Temperature according to the altimeter (°C).
    rocketState :: State,
    ematch1Present :: Bool,
    ematch2Present :: Bool,
    parachuteDeployed :: Bool,
    accelX :: Double, -- ^ Acceleration in x-axis (g).
    accelY :: Double, -- ^ Acceletation in y-axis (g).
    accelZ :: Double, -- ^ Acceleration in z-axis (g).
    pitch :: Double, -- ^ Angular velocity about pitch axis (°/s).
    yaw :: Double, -- ^ Angular velocity about yaw axis (°/s).
    roll :: Double, -- ^ Angular velocity about roll axis (°/s).
    altitude :: Double, -- ^ Altitude (m).
    gpsData :: Maybe GpsData
}

data GpsData = GpsData {
    utcTime :: TimeOfDay,
    latitude :: Double, -- ^ °
    longitude :: Double, -- ^ °
    groundSpeed :: Double, -- ^ m/s
    course :: Double, -- ^ °
    missionTimeCollected :: Word32 -- ^ ms
}

data State = Standby
           | PreFlight
           | PoweredAscent
           | CoastingAscent
           | Descent
           | Recovery
           | Undefined
    deriving (Read, Show)

rocketFrame :: Get RocketFrame
rocketFrame = do
    missionTime <- getWord32le
    -- ADC Channels
    channel0 <- getWord16le
    channel1 <- getWord16le
    channel2 <- getWord16le
    skip 8 -- channels 3-6 unused
    channel7 <- getWord16le
    let state2 = testBit channel0 15
    let state1 = testBit channel0 14
    let state0 = testBit channel0 13
    let ematch1Present = testBit channel0 12
    let ematch2Present = testBit channel0 11
    let parachuteDeployed = testBit channel0 10
    let capacitorVoltage = 0.02625071131 * fromIntegral (channel0 .&. 0x3F)
    let gpsDataValid = testBit channel1 15
    let batteryTemp = 0.00322265625 * fromIntegral (channel1 .&. 0x3F)
    let ambientTemp = 0.00322265625 * fromIntegral (channel2 .&. 0x3F)
    let batteryVoltage = 0.01434657506 * fromIntegral (channel7 .&. 0x3F)
    let rocketState = case (state2, state1, state0) of
            (False, False, False) -> Standby
            (False, False, True) -> PreFlight
            (False, True, False) -> PoweredAscent
            (False, True, True) -> CoastingAscent
            (True, False, False) -> Descent
            (True, False, True) -> Recovery
            (True, True, _) -> Undefined
    -- Accelerometer data
    accelX <- fmap ((/ 1000) . (3.9 *) . fromIntegral) getInt16le
    accelY <- fmap ((/ 1000) . (3.9 *) . fromIntegral) getInt16le
    accelZ <- fmap ((/ 1000) . (3.9 *) . fromIntegral) getInt16le
    -- Gyroscope data
    pitch <- fmap ((/ 1000) . (62.5 *) . fromIntegral) getInt16le
    roll <- fmap ((/ 1000) . (62.5 *) . fromIntegral) getInt16le
    yaw <- fmap ((/ 1000) . (62.5 *) . fromIntegral) getInt16le
    gyroTemp <- getInt8
    -- Altimeter data
    altitudeFracPart <- getInt8
    altitudeIntPart <- getInt16le
    let altitude = fromIntegral altitudeIntPart + (fromIntegral (altitudeFracPart .&. 0xF0) / 256)
    tempFracPart <- getInt8
    tempIntPart <- getInt8
    let altimeterTemp = fromIntegral tempIntPart + (fromIntegral (tempFracPart .&. 0xF0) / 256)
    gpsData <- if gpsDataValid 
        then do
            utcTime <- fmap (timeToTimeOfDay . picosecondsToDiffTime . (1000000000 *) . fromIntegral) getWord32le
            latitude <- fmap ((6e-3 *) . fromIntegral) getInt32le
            longitude <- fmap ((6e-3 *) . fromIntegral) getInt32le
            groundSpeed <- fmap ((0.00514444 *) . fromIntegral) getInt32le
            course <- fmap ((/ 100) . fromIntegral) getInt32le
            missionTimeCollected <- getWord32le
            return . Just $ GpsData {..}
        else skip 20 >> return Nothing
    return $ RocketFrame {..}

instance DefaultClass RocketFrame where
    classMembers = [
        defPropertyRO "missionTime" (return . (fromIntegral :: Word32 -> Int) . missionTime . fromObjRef),
        defPropertyRO "capacitorVoltage" (return . capacitorVoltage . fromObjRef),
        defPropertyRO "batteryVoltage" (return . batteryVoltage . fromObjRef),
        defPropertyRO "batteryTemp" (return . batteryTemp . fromObjRef),
        defPropertyRO "ambientTemp" (return . ambientTemp . fromObjRef),
        defPropertyRO "altimeterTemp" (return . altimeterTemp . fromObjRef),
        defPropertyRO "rocketState" (return . pack . show . rocketState . fromObjRef),
        defPropertyRO "ematch1Present" (return . ematch1Present . fromObjRef),
        defPropertyRO "ematch2Present" (return . ematch2Present . fromObjRef),
        defPropertyRO "parachuteDeployed" (return . parachuteDeployed . fromObjRef),
        defPropertyRO "accelX" (return . accelX . fromObjRef),
        defPropertyRO "accelY" (return . accelY . fromObjRef),
        defPropertyRO "accelZ" (return . accelZ . fromObjRef),
        defPropertyRO "pitch" (return . pitch . fromObjRef),
        defPropertyRO "yaw" (return . yaw . fromObjRef),
        defPropertyRO "roll" (return . roll . fromObjRef),
        defPropertyRO "altitude" (return . altitude . fromObjRef)]

instance DefaultClass GpsData where
    classMembers = [
        defPropertyRO "time" (return . pack . formatTime defaultTimeLocale "%T" . utcTime . fromObjRef),
        defPropertyRO "latitude" (return . latitude . fromObjRef),
        defPropertyRO "longitude" (return . longitude . fromObjRef),
        defPropertyRO "groundSpeed" (return . groundSpeed . fromObjRef),
        defPropertyRO "course" (return . course . fromObjRef),
        defPropertyRO "missionTime" (return . (fromIntegral :: Word32 -> Int) . missionTimeCollected . fromObjRef)]
