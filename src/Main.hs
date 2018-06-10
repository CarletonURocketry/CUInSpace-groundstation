module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TChan, TQueue, atomically, dupTChan, newBroadcastTChan, newTQueue, readTChan, readTQueue, writeTChan, writeTQueue)
import Control.Monad (forever, forM_)
import qualified Data.ByteString as B
import Data.Monoid ((<>))
import Data.Serialize.Get (Result(..), runGetPartial)
import Database.SQLite.Simple (withConnection, withTransaction)
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
    forM_ serialDevs $ \s -> forkIO $ serialDeviceThread s logQueue frameChan
    forkIO $ atomically (dupTChan frameChan) >>= sqliteThread sqliteDb
    loggerThread logQueue
  where argParse = info ((,) <$> many serialArg <*> dbArg)
            (progDesc "groundstation - CU In Space Ground Station")
        serialArg = strArgument
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
