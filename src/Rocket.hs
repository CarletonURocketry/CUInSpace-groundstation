{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}
module Rocket (
    RocketFrame(gpsData),
    GpsData,
    State,
    rocketFrame,
    writeGpsData,
    writeRocketFrame
) where

import Control.Monad (when)
import Data.Bits (testBit)
import Data.Int (Int8, Int16, Int64)
import Data.Monoid ((<>))
import Data.Serialize.Get (Get, getInt8, getInt16le, getInt32le, getWord8, getWord16le, getWord32le, skip)
import Data.Serialize.IEEE754 (getFloat32le)
import Data.Text (pack)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Database.SQLite.Simple (Connection, NamedParam(..), executeNamed, lastInsertRowId)
import Graphics.QML (DefaultClass(..), defPropertyConst, fromObjRef)

data RocketFrame = RocketFrame {
    missionTime :: Word32, -- ^ Time since initialization at which the packet was sent (ms).
    accelerometerTemp :: Double, -- ^ Temperature according to accelerometer (°C).
    altimeterTemp :: Double, -- ^ Temperature according to the altimeter (°C).
    rocketState :: State,
    parachuteDeployed :: Bool,
    accelX :: Double, -- ^ Acceleration in x-axis (g).
    accelY :: Double, -- ^ Acceletation in y-axis (g).
    accelZ :: Double, -- ^ Acceleration in z-axis (g).
    altitude :: Float, -- ^ Altitude (m).
    signalNoise :: Int8, -- ^ Signal to noise ratio.
    gpsData :: Maybe GpsData
} deriving (Typeable)

data GpsData = GpsData {
    utcTime :: UTCTime,
    latitude :: Double, -- ^ °
    longitude :: Double, -- ^ °
    groundSpeed :: Double, -- ^ m/s
    course :: Double, -- ^ °
    gpsAltitude :: Double -- ^ m
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
    initByte <- getWord8
    when (initByte /= 0x52) $ fail "Not a valid packet (no start byte)."
    skip 3 -- Unimportant header stuff
    missionTime <- getWord32le
    -- Flags
    flags <- getWord16le
    let parachuteDeployed = testBit flags 0
    let gpsDataValid = testBit flags 1
    let rocketState = case fmap (testBit flags) [10..15] of
            [True,  False, False, False, False, False] -> Standby
            [False, True,  False, False, False, False] -> PreFlight
            [False, False, True,  False, False, False] -> PoweredAscent
            [False, False, False, True,  False, False] -> CoastingAscent
            [False, False, False, False, True,  False] -> Descent
            [False, False, False, False, False, True ] -> Recovery
            _ -> Undefined
    -- ADC data unused
    skip 16
    -- Accelerometer data
    accelX <- fmap (((16/int16Range) *) . fromIntegral) getInt16le
    accelY <- fmap (((16/int16Range) *) . fromIntegral) getInt16le
    accelZ <- fmap (((16/int16Range) *) . fromIntegral) getInt16le
    accelerometerTemp <- fmap fromIntegral getInt16le
    -- Altimeter data
    altimeterTemp <- fmap ((/ 100) . fromIntegral) getInt32le
    altitude <- getFloat32le
    gpsData <- if gpsDataValid
        then do
            utcTime <- fmap (posixSecondsToUTCTime . fromIntegral) getWord32le
            latitude <- fmap ((/ 60) . (1e-4 *) . fromIntegral) getInt32le
            longitude <- fmap ((/ 60) . (1e-4 *) . fromIntegral) getInt32le
            groundSpeed <- fmap ((0.00514444 *) . fromIntegral) getInt16le
            course <- fmap ((/ 100) . fromIntegral) getInt16le
            gpsAltitude <- fmap ((/ 100) . fromIntegral) getInt32le
            return . Just $ GpsData {..}
        else skip 20 >> return Nothing
    finiByte <- getWord8
    when (finiByte /= 0xCC) $ fail "Not a valid packet (no end byte)."
    signalNoise <- getInt8
    return $ RocketFrame {..}
  where int16Range = fromIntegral ((maxBound - minBound) :: Int16)

instance DefaultClass RocketFrame where
    classMembers = [
        defPropertyConst "missionTime" (return . (fromIntegral :: Word32 -> Int) . missionTime . fromObjRef),
        defPropertyConst "accelerometerTemp" (return . accelerometerTemp . fromObjRef),
        defPropertyConst "altimeterTemp" (return . altimeterTemp . fromObjRef),
        defPropertyConst "state" (return . pack . show . rocketState . fromObjRef),
        defPropertyConst "parachuteDeployed" (return . parachuteDeployed . fromObjRef),
        defPropertyConst "accelX" (return . accelX . fromObjRef),
        defPropertyConst "accelY" (return . accelY . fromObjRef),
        defPropertyConst "accelZ" (return . accelZ . fromObjRef),
        defPropertyConst "altitude" (return . (realToFrac :: Float -> Double) . altitude . fromObjRef),
        defPropertyConst "signalNoise" (return . (fromIntegral :: Int8 -> Int) . signalNoise . fromObjRef)]

instance DefaultClass GpsData where
    classMembers = [
        defPropertyConst "time" (return . pack . formatTime defaultTimeLocale "%T" . utcTime . fromObjRef),
        defPropertyConst "latitude" (return . latitude . fromObjRef),
        defPropertyConst "longitude" (return . longitude . fromObjRef),
        defPropertyConst "groundSpeed" (return . groundSpeed . fromObjRef),
        defPropertyConst "course" (return . course . fromObjRef),
        defPropertyConst "altitude" (return . gpsAltitude . fromObjRef)]

writeGpsData :: GpsData -> Connection -> IO Int64
writeGpsData (GpsData {..}) conn = do
    executeNamed conn gpsQuery [
        ":time" := (pack . formatTime defaultTimeLocale "%T%Q" $ utcTime),
        ":lat" := latitude,
        ":long" := longitude,
        ":gs" := groundSpeed,
        ":course" := course,
        ":alt" := gpsAltitude]
    lastInsertRowId conn
  where gpsQuery = "INSERT INTO GPS " <>
            "(GPS_Time, Latitude, Longitude, Speed, Course, GPS_Altitude) " <>
            "VALUES (:time, :lat, :long, :gs, :course, :alt);"

writeRocketFrame :: RocketFrame -> Connection -> IO ()
writeRocketFrame (RocketFrame {..}) conn = case gpsData of
    Just gd -> do
        rid <- writeGpsData gd conn
        executeNamed conn withGpsQuery [
            ":mt" := missionTime,
            ":altt" := altimeterTemp,
            ":accelt" := accelerometerTemp,
            ":st" := (pack . show $ rocketState),
            ":par" := parachuteDeployed,
            ":ax" := accelX,
            ":ay" := accelY,
            ":az" := accelZ,
            ":alt" := altitude,
            ":sn" := signalNoise,
            ":rid" := rid]
    Nothing -> executeNamed conn withoutGpsQuery [
            ":mt" := missionTime,
            ":altt" := altimeterTemp,
            ":accelt" := accelerometerTemp,
            ":st" := (pack . show $ rocketState),
            ":par" := parachuteDeployed,
            ":ax" := accelX,
            ":ay" := accelY,
            ":az" := accelZ,
            ":alt" := altitude,
            ":sn" := signalNoise]
  where withGpsQuery = "INSERT INTO Rocket_Telemetry " <>
            "(Mission_Time, State, Parachute_Deployed, Acceleration_X," <>
            "Acceleration_Y, Acceleration_Z, Altitude, Alt_Temp, Accel_Temp," <>
            "Signal_To_Noise, GPS_Data) SELECT :mt, :st, :par, :ax, :ay, :az,"<>
            ":alt, :altt, :accelt, :sn, FrameID FROM GPS WHERE rowid == :rid;"
        withoutGpsQuery = "INSERT INTO Rocket_Telemetry " <>
            "(Mission_Time, State, Parachute_Deployed, Acceleration_X, " <>
            "Acceleration_Y, Acceleration_Z, Altitude, Alt_Temp, Accel_Temp," <>
            "Signal_To_Noise, GPS_Data) VALUES(:mt, :st, :par, :ax, :ay, :az,"<>
            ":alt, :altt, :accelt, :sn, NULL);"
