module Web.Application where

import Control.Monad.Except (ExceptT)
import Servant
import Web.Routes (WidgetsApi, widgetsApiHandler)

type RouteHandler = ExceptT ServerError IO

toHandler :: RouteHandler a -> Handler a
toHandler = Handler

app :: Application
app =
  serveWithContextT
    (Proxy :: Proxy WidgetsApi)
    EmptyContext
    toHandler
    widgetsApiHandler

