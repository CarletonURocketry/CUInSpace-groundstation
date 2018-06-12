{-# LANGUAGE DeriveDataTypeable, LambdaCase, RecordWildCards #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever, forM_)
import qualified Data.ByteString as B
import Data.Monoid ((<>))
import Data.Serialize.Get (Result(..), runGetPartial)
import Data.Typeable (Typeable)
import Database.SQLite.Simple (withConnection, withTransaction)
import Graphics.QML
import Options.Applicative
import Pipes
import Pipes.ByteString (fromHandle)
import System.IO (hClose)
import System.Serial (BaudRate(..), FlowControl(..), Parity(..), StopBits(..), openSerial)

import Payload
import Rocket
import XBee

main :: IO ()
main = do
    (serialDevs, sqliteDb) <- execParser argParse
    (logQueue, frameChan) <- atomically $ (,) <$> newTQueue <*> newBroadcastTChan
    ctx <- newContext
    forkIO $ loggerThread logQueue
    forkIO $ atomically (dupTChan frameChan) >>= sqliteThread sqliteDb
    forkIO $ atomically (dupTChan frameChan) >>= qmlThread ctx
    forM_ serialDevs $ \s -> forkIO $ serialDeviceThread s logQueue frameChan
    runEngineLoop (defaultEngineConfig{contextObject = Just (anyObjRef ctx)})

argParse :: ParserInfo ([String], String)
argParse = info ((,) <$> some serialArg <*> dbArg)
    (progDesc "groundstation - CU In Space Ground Station")
  where serialArg = strArgument
            (help "Serial devices to read from." <> metavar "FILE")
        dbArg = strOption
            (short 'l' <> long "log" <> help "SQLite database to log to." <>
            metavar "FILE" <> value "log.db")

serialDeviceThread :: String -> TQueue String -> TChan Frame -> IO ()
serialDeviceThread dev logger frames = do
    h <- openSerial dev B9600 8 One NoParity NoFlowControl
    runEffect $ fromHandle h >-> (await >>= nextFrame)
    hClose h
  where nextFrame bs = case B.elemIndex 0x52 bs of
            Just i -> resPipe $ runGetPartial xBeeFrame (B.drop i bs)
            Nothing -> await >>= nextFrame
        resPipe (Fail err rest) = enlog ("Error parsing frame: " ++ err) >> nextFrame rest
        resPipe (Partial f) = await >>= (resPipe . f)
        resPipe (Done res rest) = do
            case res of
                GoodFrame RocketAuxilliary -> enlog "Got rocket auxilliary frame somehow."
                GoodFrame f -> void . liftIO . atomically . writeTChan frames $ f
                NotForUs -> enlog "Got frame intended for another recipient."
                InvalidSource s -> enlog $ "Invalid frame source: " ++ show s ++ "."
                InvalidType t -> enlog $ "Invalid frame type: " ++ show t ++ "."
                ImpossibleSource s -> enlog $ "Frame source " ++ show s ++ " produced impossible frame type."
            nextFrame rest
        enlog = void . liftIO . atomically . writeTQueue logger

sqliteThread :: String -> TChan Frame -> IO ()
sqliteThread db frames = withConnection db $ \conn -> forever $ do
    fr <- atomically (readTChan frames)
    withTransaction conn $ case fr of
        RocketPrimary rf -> writeRocketFrame rf conn
        RocketAuxilliary -> return ()
        UAVTelemetry pf -> writePayloadFrame pf conn
        ContainerTelemetry cf -> writeContainerFrame cf conn

loggerThread :: TQueue String -> IO ()
loggerThread logs = forever $ atomically (readTQueue logs) >>= putStrLn

data Context = Context {
    rocketSt :: TVar (Maybe RocketFrame),
    rocketSig :: SignalKey (IO ()),
    rocketGps :: TVar (Maybe GpsData),
    gpsSig :: SignalKey (IO ()),
    containerSt :: TVar (Maybe ContainerFrame),
    containerSig :: SignalKey (IO ()),
    payloadSt :: TVar (Maybe PayloadFrame),
    payloadSig :: SignalKey (IO ())
} deriving (Typeable)

newContext :: IO (ObjRef Context)
newContext = do
    rocketSt <- atomically $ newTVar Nothing
    rocketSig <- newSignalKey
    rocketGps <- atomically $ newTVar Nothing
    gpsSig <- newSignalKey
    containerSt <- atomically $ newTVar Nothing
    containerSig <- newSignalKey
    payloadSt <- atomically $ newTVar Nothing
    payloadSig <- newSignalKey
    cls <- newClass [
         defPropertySigRO "rocket" rocketSig (getTVar rocketSt),
         defPropertySigRO "rocketGps" gpsSig (getTVar rocketGps),
         defPropertySigRO "container" containerSig (getTVar containerSt),
         defPropertySigRO "payload" payloadSig (getTVar payloadSt)]
    newObject cls (Context {..})
  where getTVar :: DefaultClass a => TVar (Maybe a) -> ObjRef Context -> IO (Maybe (ObjRef a))
        getTVar tv _ = atomically (readTVar tv) >>= \case
            Just v -> fmap Just (newObjectDC v)
            Nothing -> return Nothing

qmlThread :: ObjRef Context -> TChan Frame -> IO ()
qmlThread ref frames = forever $ atomically (readTChan frames) >>= \case
        RocketPrimary rf -> do
            atomically $ writeTVar rocketSt (Just rf)
            case gpsData rf of
                Just gps -> atomically (writeTVar rocketGps (Just gps)) >> fireSignal gpsSig ref
                Nothing -> return ()
            fireSignal rocketSig ref
        RocketAuxilliary -> return ()
        UAVTelemetry pf -> atomically (writeTVar payloadSt (Just pf)) >> fireSignal containerSig ref
        ContainerTelemetry cf -> atomically (writeTVar containerSt (Just cf)) >> fireSignal containerSig ref
  where (Context {..}) = fromObjRef ref
