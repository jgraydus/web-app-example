module Widgets where

import Control.Concurrent.STM.TVar (readTVar, TVar)
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

