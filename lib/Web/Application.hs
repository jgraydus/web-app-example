module Web.Application where

import Servant
import Web.Routes (WidgetsApi, widgetsApiHandler)

app :: Application
app =
  serve
    (Proxy :: Proxy WidgetsApi)
    widgetsApiHandler

