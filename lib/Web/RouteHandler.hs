module Web.RouteHandler (
  RouteHandler
) where

import Control.Monad.Except (MonadError)
import Servant (ServerError, ServerT)
import Logger (HasLogger)
import Widgets

type RouteHandler api = forall m . Constraints m => ServerT api m

type Constraints m =
  ( HasLogger m
  , Monad m
  , MonadError ServerError m
  , WidgetService m
  )

