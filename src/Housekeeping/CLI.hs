{-# LANGUAGE OverloadedStrings #-}

module Housekeeping.CLI (main, formatAccessLog) where

import Data.Pool
import Database.PostgreSQL.Simple
import Housekeeping.API
import Housekeeping.Prelude
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import qualified RIO.List as L
import System.Environment (lookupEnv)

formatAccessLog :: Request -> Status -> Maybe Integer -> Utf8Builder
formatAccessLog req status mFileSize =
  "Access"
    <> " source:"
    <> addr
    <> " method:"
    <> method
    <> " path:"
    <> path
    <> " code:"
    <> code
    <> " length:"
    <> size
  where
    method = displayBytesUtf8 $ requestMethod req
    path = displayBytesUtf8 $ rawPathInfo req
    code = display $ statusCode status
    size = maybe "-" display mFileSize
    headers = requestHeaders req
    ipHeaders = ["x-real-ip", "x-forwarded-for"]
    addr =
      headers
        & L.find (\(key, _) -> L.elem key ipHeaders)
        & maybe "" snd
        & displayBytesUtf8

appConnectInfo :: IO ConnectInfo
appConnectInfo = do
  host <- fromMaybe "localhost" <$> lookupEnv "DB_HOST"
  port <- fromMaybe 5432 . (>>= readMaybe) <$> lookupEnv "DB_PORT"
  user <- fromMaybe "test" <$> lookupEnv "DB_USER"
  name <- fromMaybe "testdb" <$> lookupEnv "DB_NAME"
  pass <- fromMaybe "test" <$> lookupEnv "DB_PASS"
  pure
    ConnectInfo
      { connectHost = host,
        connectPort = port,
        connectUser = user,
        connectPassword = pass,
        connectDatabase = name
      }

mkDataSource :: IO (Pool Connection)
mkDataSource = do
  cInfo <- appConnectInfo
  createPool (connect cInfo) close 1 0.5 10

main :: IO ()
main = do
  logOptions <- setLogUseTime True <$> logOptionsHandle stderr True
  withLogFunc logOptions $ \lf -> do
    ds <- mkDataSource
    let env = mkEnv lf ds
    runRIO env $ logInfo "Server started"
    let warpLogger req status mFileSize =
          runRIO env $ logInfo $ formatAccessLog req status mFileSize
    let settings =
          defaultSettings
            & setLogger warpLogger
            & setPort 8080
    runSettings settings (app env)
