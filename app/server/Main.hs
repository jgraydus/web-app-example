module Main where

import Network.Wai.Handler.Warp (run)
import Web.Application (newApp)

main :: IO ()
main = do
  let port = 8000
  app <- newApp
  run port app

