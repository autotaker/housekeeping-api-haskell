module Housekeeping.CLI(main) where

import           Housekeeping.API
import           Network.Wai.Handler.Warp

main :: IO ()
main = run 8080 app
