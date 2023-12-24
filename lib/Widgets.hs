module Widgets where

import Control.Concurrent.STM.TVar (modifyTVar', readTVar, TVar, writeTVar)
import Control.Concurrent.STM (atomically)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (asks, MonadReader)
import Data.Aeson (ToJSON)
import Data.List (find)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Records (getField, HasField)

type WidgetId = Int
type WidgetName = Text

data Widget = Widget
  { id :: WidgetId
  , name :: WidgetName
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

class Monad m => WidgetService m where
  getWidgets :: m [Widget]
  getWidget :: WidgetId -> m (Maybe Widget)
  createWidget :: WidgetName -> m Widget
  updateWidget :: WidgetId -> WidgetName -> m (Maybe Widget)
  deleteWidget :: WidgetId -> m ()

instance
  ( Monad m
  , MonadIO m
  , MonadReader r m
  , HasField "widgetsDb" r (TVar [Widget])
  ) => WidgetService m where

  getWidgets = do
    widgetsDb <- asks (getField @"widgetsDb")
    liftIO $ atomically $ readTVar widgetsDb

  getWidget widgetId = do
    widgets <- getWidgets
    pure $ find (\widget -> widget.id == widgetId) widgets

  createWidget widgetName = do
    widgetsDb <- asks (getField @"widgetsDb")
    liftIO $ atomically $ do
      widgets <- readTVar widgetsDb
      let nextId =
            if null widgets
            then 0
            else 1 + maximum (fmap (\widget -> widget.id) widgets)
          newWidget = Widget { id = nextId, name = widgetName }
      writeTVar widgetsDb (newWidget : widgets)
      pure newWidget

  updateWidget widgetId widgetName = do
    widgetsDb <- asks (getField @"widgetsDb")
    liftIO $ atomically $ do
      widgets <- readTVar widgetsDb
      case find (\widget -> widget.id == widgetId) widgets of
        Nothing -> pure Nothing
        Just widget -> do
          let updatedWidget = widget { name = widgetName }
          writeTVar widgetsDb
            (updatedWidget :
              (filter (\widget -> widget.id /= widgetId) widgets))
          pure $ Just updatedWidget

  deleteWidget widgetId = do
    widgetsDb <- asks (getField @"widgetsDb")
    liftIO $ atomically $
      modifyTVar' widgetsDb (filter (\widget -> widget.id /= widgetId))

