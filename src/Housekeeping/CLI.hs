{-# LANGUAGE OverloadedStrings #-}
module Housekeeping.CLI(main) where

import           RIO

import           Housekeeping.API
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.Warp


main :: IO ()
main = do
    logOptions <- setLogUseTime True <$> logOptionsHandle stderr True
    withLogFunc logOptions $ \lf -> do
        simpleApp <- mkSimpleApp lf Nothing
        runRIO simpleApp $ logInfo "Server started"
        let warpLogger req status mFileSize = runRIO simpleApp $ logInfo $
                displayShow method <> ":"
                <> display code <> ":"
                <> maybe "-" display mFileSize <> ":"
                <> displayShow path
                where
                method = requestMethod req
                code = statusCode status
                path = pathInfo req


        let settings = defaultSettings
                     & setLogger warpLogger
                     & setPort 8080
        runSettings settings (app simpleApp)
