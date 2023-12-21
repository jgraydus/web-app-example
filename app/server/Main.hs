module Main where

import Network.Wai.Handler.Warp (run)
import Web.Application (app)

main :: IO ()
main = run port app
  where port = 8000

