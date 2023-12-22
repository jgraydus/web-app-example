module Widgets where

import Data.Aeson (ToJSON)
import Data.List (find)
import Data.Text (Text)
import GHC.Generics (Generic)

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

widgetsDb :: [Widget]
widgetsDb =
  [ Widget { id = 42, name = "Large Widget" }
  , Widget { id = 17, name = "Small Widget" }
  ]

instance Monad m => WidgetService m where
  getWidgets = pure widgetsDb
  getWidget widgetId = pure $ find (\widget -> widget.id == widgetId) widgetsDb

