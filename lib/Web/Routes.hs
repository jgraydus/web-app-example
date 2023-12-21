module Web.Routes (
  WidgetsApi,
  widgetsApiHandler,
) where

import Data.List (find)
import Data.Text (Text)
import Servant

type WidgetId = Int
type WidgetName = Text
type Widget = (WidgetId, WidgetName)

type WidgetsApi = GetWidgets :<|> GetWidget

widgetsApiHandler :: Server WidgetsApi
widgetsApiHandler = getWidgetsHandler :<|> getWidgetHandler

widgetsDb :: [Widget]
widgetsDb = [(42, "Large Widget"), (17, "Small Widget")]

--------------------------------------------------
-- GET /widget

type GetWidgets = "widget" :> Get '[JSON] [Widget]

getWidgetsHandler :: Server GetWidgets
getWidgetsHandler = pure widgetsDb

--------------------------------------------------
-- GET /widget/:widgetId

type GetWidget = "widget" :> Capture "widgetId" WidgetId :> Get '[JSON] Widget

getWidgetHandler :: Server GetWidget
getWidgetHandler widgetId =
   case find ((== widgetId) . fst) widgetsDb of
     Just widget -> pure widget
     Nothing -> throwError err404

