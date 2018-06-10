module Main where

import Control.Concurrent.STM (TChan, TQueue, atomically, readTChan, writeTChan, writeTQueue)
import Control.Monad (forever)
import qualified Data.ByteString as B
import Data.Serialize.Get (Result(..), runGetPartial)
import Database.SQLite.Simple (withConnection, withTransaction)
import Pipes
import Pipes.ByteString (fromHandle)
import System.IO (hClose)
import System.Serial (BaudRate(..), FlowControl(..), Parity(..), StopBits(..), openSerial)

import Payload
import Rocket
import XBee

main :: IO ()
main = return ()

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
