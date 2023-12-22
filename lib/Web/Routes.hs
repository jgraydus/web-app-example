module Web.Routes (
  WidgetsApi,
  widgetsApiHandler,
) where

import Data.Aeson (ToJSON)
import Data.List (find)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
import Web.RouteHandler

type WidgetId = Int
type WidgetName = Text

data Widget = Widget { id :: WidgetId, name :: WidgetName }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

type WidgetsApi = GetWidgets :<|> GetWidget

widgetsApiHandler :: RouteHandler WidgetsApi
widgetsApiHandler = getWidgetsHandler :<|> getWidgetHandler

widgetsDb :: [Widget]
widgetsDb =
  [ Widget { id = 42, name = "Large Widget" }
  , Widget { id = 17, name = "Small Widget" }
  ]

--------------------------------------------------
-- GET /widget

type GetWidgets = "widget" :> Get '[JSON] [Widget]

getWidgetsHandler :: RouteHandler GetWidgets
getWidgetsHandler = pure widgetsDb

--------------------------------------------------
-- GET /widget/:widgetId

type GetWidget = "widget" :> Capture "widgetId" WidgetId :> Get '[JSON] Widget

getWidgetHandler :: RouteHandler GetWidget
getWidgetHandler widgetId =
   case find (\widget -> widget.id == widgetId)  widgetsDb of
     Just widget -> pure widget
     Nothing -> throwError err404

