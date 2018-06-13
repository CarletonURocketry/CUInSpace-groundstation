{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}
module Rocket (
    RocketFrame(gpsData),
    GpsData(..),
    State(..),
    rocketFrame,
    writeGpsData,
    writeRocketFrame
) where

import Data.Bits ((.&.), shiftR, testBit)
import Data.Int (Int8, Int64)
import Data.Monoid ((<>))
import Data.Serialize.Get (Get, getInt8, getInt16le, getInt32le, getWord16le, getWord32le, skip)
import Data.Text (pack)
import Data.Time (TimeOfDay, defaultTimeLocale, formatTime, picosecondsToDiffTime, timeToTimeOfDay)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Database.SQLite.Simple (Connection, NamedParam(..), executeNamed, lastInsertRowId)
import Graphics.QML (DefaultClass(..), defPropertyConst, fromObjRef)

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
} deriving (Typeable)

data GpsData = GpsData {
    utcTime :: TimeOfDay,
    latitude :: Double, -- ^ °
    longitude :: Double, -- ^ °
    groundSpeed :: Double, -- ^ m/s
    course :: Double, -- ^ °
    missionTimeCollected :: Word32 -- ^ ms
} deriving (Typeable)

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
    let state2 = testBit channel0 0
    let state1 = testBit channel0 1
    let state0 = testBit channel0 2
    let ematch1Present = testBit channel0 3
    let ematch2Present = testBit channel0 4
    let parachuteDeployed = testBit channel0 5
    let capacitorVoltage = 0.02625071131 * fromIntegral (shiftR channel0 6)
    let gpsDataValid = testBit channel1 0
    let batteryTemp = 0.00322265625 * fromIntegral (shiftR channel1 6)
    let ambientTemp = 0.00322265625 * fromIntegral (shiftR channel2 6)
    let batteryVoltage = 0.01434657506 * fromIntegral (shiftR channel7 6)
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
            latitude <- fmap ((/ 60) . (1e-4 *) . fromIntegral) getInt32le
            longitude <- fmap ((/ 60) . (1e-4 *) . fromIntegral) getInt32le
            groundSpeed <- fmap ((0.00514444 *) . fromIntegral) getInt16le
            course <- fmap ((/ 100) . fromIntegral) getInt16le
            missionTimeCollected <- getWord32le
            return . Just $ GpsData {..}
        else skip 20 >> return Nothing
    return $ RocketFrame {..}

instance DefaultClass RocketFrame where
    classMembers = [
        defPropertyConst "missionTime" (return . (fromIntegral :: Word32 -> Int) . missionTime . fromObjRef),
        defPropertyConst "capacitorVoltage" (return . capacitorVoltage . fromObjRef),
        defPropertyConst "batteryVoltage" (return . batteryVoltage . fromObjRef),
        defPropertyConst "batteryTemp" (return . batteryTemp . fromObjRef),
        defPropertyConst "ambientTemp" (return . ambientTemp . fromObjRef),
        defPropertyConst "altimeterTemp" (return . altimeterTemp . fromObjRef),
        defPropertyConst "state" (return . pack . show . rocketState . fromObjRef),
        defPropertyConst "ematch1Present" (return . ematch1Present . fromObjRef),
        defPropertyConst "ematch2Present" (return . ematch2Present . fromObjRef),
        defPropertyConst "parachuteDeployed" (return . parachuteDeployed . fromObjRef),
        defPropertyConst "accelX" (return . accelX . fromObjRef),
        defPropertyConst "accelY" (return . accelY . fromObjRef),
        defPropertyConst "accelZ" (return . accelZ . fromObjRef),
        defPropertyConst "pitch" (return . pitch . fromObjRef),
        defPropertyConst "yaw" (return . yaw . fromObjRef),
        defPropertyConst "roll" (return . roll . fromObjRef),
        defPropertyConst "altitude" (return . altitude . fromObjRef)]

instance DefaultClass GpsData where
    classMembers = [
        defPropertyConst "time" (return . pack . formatTime defaultTimeLocale "%T" . utcTime . fromObjRef),
        defPropertyConst "latitude" (return . latitude . fromObjRef),
        defPropertyConst "longitude" (return . longitude . fromObjRef),
        defPropertyConst "groundSpeed" (return . groundSpeed . fromObjRef),
        defPropertyConst "course" (return . course . fromObjRef),
        defPropertyConst "missionTime" (return . (fromIntegral :: Word32 -> Int) . missionTimeCollected . fromObjRef)]

writeGpsData :: GpsData -> Connection -> IO Int64
writeGpsData (GpsData {..}) conn = do
    executeNamed conn gpsQuery [
        ":time" := (pack . formatTime defaultTimeLocale "%T%Q" $ utcTime),
        ":lat" := latitude,
        ":long" := longitude,
        ":gs" := groundSpeed,
        ":course" := course,
        ":mtime" := missionTimeCollected]
    lastInsertRowId conn
  where gpsQuery = "INSERT INTO GPS " <>
            "(GPS_Time, Latitude, Longitude, Speed, Course, Mission_Time) " <>
            "VALUES (:time, :lat, :long, :gs, :course, :mtime);"

writeRocketFrame :: RocketFrame -> Connection -> IO ()
writeRocketFrame (RocketFrame {..}) conn = case gpsData of
    Just gd -> do
        rid <- writeGpsData gd conn
        executeNamed conn withGpsQuery [
            ":mt" := missionTime,
            ":cv" := capacitorVoltage,
            ":bv" := batteryVoltage,
            ":bt" := batteryTemp,
            ":ambt" := ambientTemp,
            ":gt" := gyroTemp,
            ":altt" := altimeterTemp,
            ":st" := (pack . show $ rocketState),
            ":e1" := ematch1Present,
            ":e2" := ematch2Present,
            ":par" := parachuteDeployed,
            ":ax" := accelX,
            ":ay" := accelY,
            ":az" := accelZ,
            ":p" := pitch,
            ":y" := yaw,
            ":r" := roll,
            ":alt" := altitude,
            ":rid" := rid]
    Nothing -> executeNamed conn withoutGpsQuery [
            ":mt" := missionTime,
            ":cv" := capacitorVoltage,
            ":bv" := batteryVoltage,
            ":bt" := batteryTemp,
            ":ambt" := ambientTemp,
            ":gt" := gyroTemp,
            ":altt" := altimeterTemp,
            ":st" := (pack . show $ rocketState),
            ":e1" := ematch1Present,
            ":e2" := ematch2Present,
            ":par" := parachuteDeployed,
            ":ax" := accelX,
            ":ay" := accelY,
            ":az" := accelZ,
            ":p" := pitch,
            ":y" := yaw,
            ":r" := roll,
            ":alt" := altitude]
  where withGpsQuery = "INSERT INTO Rocket_Telemetry " <>
            "(Mission_Time, Capacitor_Voltage, Battery_Voltage, Battery_Temp, Ambient_Temp, " <>
            "State, E_Match_1_Present, E_Match_2_Present, Parachute_Deployed, Acceleration_X, " <>
            "Acceleration_Y, Acceleration_Z, Pitch_Rate, Roll_Rate, Yaw_Rate, Gyro_Temp, " <>
            "Altitude, Alt_Temp, GPS_Data) " <>
            "SELECT :mt, :cv, :bv, :bt, :ambt, :st, :e1, :e2, :par, :ax, :ay, :az, :p, :r, :y, " <>
            ":gt, :alt, :altt, FrameID " <>
            "FROM GPS WHERE rowid == :rid;"
        withoutGpsQuery = "INSERT INTO Rocket_Telemetry " <>
            "(Mission_Time, Capacitor_Voltage, Battery_Voltage, Battery_Temp, Ambient_Temp, " <>
            "State, E_Match_1_Present, E_Match_2_Present, Parachute_Deployed, Acceleration_X, " <>
            "Acceleration_Y, Acceleration_Z, Pitch_Rate, Roll_Rate, Yaw_Rate, Gyro_Temp, " <>
            "Altitude, Alt_Temp, GPS_Data) " <>
            "VALUES(:mt, :cv, :bv, :bt, :ambt, :st, :e1, :e2, :par, :ax, :ay, :az, :p, :r, :y, " <>
            ":gt, :alt, :altt, NULL);"
