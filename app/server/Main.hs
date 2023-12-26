module Main where

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Logger (LogLevel(DEBUG,INFO), Logger(..), toLogStr, withLogger)
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Application (newApp)

getLogLevel :: IO LogLevel
getLogLevel = lookupEnv "LOG_LEVEL" <&> (>>= readMaybe) <&> fromMaybe DEBUG

main :: IO ()
main = do
  let port = 8000

  logLevel <- getLogLevel

  withLogger logLevel $ \logger@Logger { runLogger } -> do
    runLogger INFO $ "LOG_LEVEL=" <> toLogStr (show logLevel)
    runLogger INFO "initializing..."
    app <- newApp logger
    runLogger INFO $ "starting server on port " <> toLogStr port
    run port app

