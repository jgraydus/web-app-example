module Web.RouteHandler (
  RouteHandler
) where

import Control.Monad.Except (MonadError)
import Servant (ServerError, ServerT)

type RouteHandler api = forall m . Constraints m => ServerT api m

type Constraints m =
  ( Monad m
  , MonadError ServerError m
  )

