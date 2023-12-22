module Web.Routes (
  WidgetsApi,
  widgetsApiHandler,
) where

import Servant
import Widgets
import Web.RouteHandler

type WidgetsApi = GetWidgets :<|> GetWidget

widgetsApiHandler :: RouteHandler WidgetsApi
widgetsApiHandler = getWidgetsHandler :<|> getWidgetHandler

--------------------------------------------------
-- GET /widget

type GetWidgets = "widget" :> Get '[JSON] [Widget]

getWidgetsHandler :: RouteHandler GetWidgets
getWidgetsHandler = getWidgets

--------------------------------------------------
-- GET /widget/:widgetId

type GetWidget = "widget" :> Capture "widgetId" WidgetId :> Get '[JSON] Widget

getWidgetHandler :: RouteHandler GetWidget
getWidgetHandler widgetId = do
   result <- getWidget widgetId
   case result of
     Just widget -> pure widget
     Nothing -> throwError err404

