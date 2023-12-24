module Web.Routes (
  WidgetsApi,
  widgetsApiHandler,
) where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Servant
import Widgets
import Web.RouteHandler

type WidgetsApi =
       GetWidgets
  :<|> GetWidget
  :<|> CreateWidget
  :<|> UpdateWidget
  :<|> DeleteWidget

widgetsApiHandler :: RouteHandler WidgetsApi
widgetsApiHandler =
       getWidgetsHandler
  :<|> getWidgetHandler
  :<|> createWidgetHandler
  :<|> updateWidgetHandler
  :<|> deleteWidgetHandler

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

--------------------------------------------------
-- POST /widget

newtype CreateWidgetReqBody = CreateWidgetReqBody { name :: WidgetName }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

type CreateWidget =
     "widget"
  :> ReqBody '[JSON] CreateWidgetReqBody
  :> Post '[JSON] Widget

createWidgetHandler :: RouteHandler CreateWidget
createWidgetHandler CreateWidgetReqBody { name } = createWidget name

--------------------------------------------------
-- PATCH /widget/:widgetId

newtype UpdateWidgetReqBody = UpdateWidgetReqBody { name :: WidgetName }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

type UpdateWidget =
     "widget"
  :> Capture "widgetId" WidgetId
  :> ReqBody '[JSON] UpdateWidgetReqBody
  :> Patch '[JSON] Widget

updateWidgetHandler :: RouteHandler UpdateWidget
updateWidgetHandler widgetId UpdateWidgetReqBody { name } = do
  result <- updateWidget widgetId name
  case result of
    Just widget -> pure widget
    Nothing -> throwError err404

--------------------------------------------------
-- DELETE /widget/:widgetId

type DeleteWidget =
     "widget"
  :> Capture "widgetId" WidgetId
  :> Delete '[JSON] ()

deleteWidgetHandler :: RouteHandler DeleteWidget
deleteWidgetHandler widgetId = deleteWidget widgetId

