{-# LANGUAGE OverloadedStrings #-}
module Housekeeping.CLI(main, formatAccessLog) where

import           RIO
import qualified RIO.List                  as L

import           Housekeeping.API
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.Warp

formatAccessLog :: Request -> Status -> Maybe Integer -> Utf8Builder
formatAccessLog req status mFileSize =
    "Access"
        <> " source:" <> addr
        <> " method:" <> method
        <> " path:" <> path
        <> " code:" <> code
        <> " length:" <> size
    where
    method = displayBytesUtf8 $ requestMethod req
    path = displayBytesUtf8 $ rawPathInfo req
    code = display $ statusCode status
    size = maybe "-" display mFileSize
    headers = requestHeaders req
    ipHeaders = ["x-real-ip", "x-forwarded-for"]
    addr = headers
        & L.find (\(key, _) -> L.elem key ipHeaders)
        & maybe "" snd
        & displayBytesUtf8

main :: IO ()
main = do
    logOptions <- setLogUseTime True <$> logOptionsHandle stderr True
    withLogFunc logOptions $ \lf -> do
        simpleApp <- mkSimpleApp lf Nothing
        runRIO simpleApp $ logInfo "Server started"
        let warpLogger req status mFileSize =
                runRIO simpleApp $ logInfo $ formatAccessLog req status mFileSize
        let settings = defaultSettings
                     & setLogger warpLogger
                     & setPort 8080
        runSettings settings (app simpleApp)
