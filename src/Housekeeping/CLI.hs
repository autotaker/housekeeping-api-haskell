{-# LANGUAGE OverloadedStrings #-}
module Housekeeping.CLI(main) where

import           RIO

import           Housekeeping.API
import           Network.Wai.Handler.Warp

main :: IO ()
main = do
    logOptions <- setLogUseTime True <$> logOptionsHandle stderr True
    withLogFunc logOptions $ \lf -> do
        simpleApp <- mkSimpleApp lf Nothing
        runSimpleApp $ logInfo "Server started"
        run 8080 (app simpleApp)
