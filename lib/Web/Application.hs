module Web.Application where

import Control.Concurrent.STM.TVar (newTVarIO, TVar)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Except (ExceptT)
import Logger (Logger)
import Servant
import Widgets (Widget(..))
import Web.RequestContext
import Web.Routes (WidgetsApi, widgetsApiHandler)

type RouteHandler = ReaderT RequestContext (ExceptT ServerError IO)

toHandler :: RequestContext -> RouteHandler a -> Handler a
toHandler ctx m = Handler (runReaderT m ctx)

app :: Logger -> TVar [Widget] -> Application
app logger widgetsDb req res = do
  let ctx = RequestContext { logger, widgetsDb }

  serveWithContextT
    (Proxy :: Proxy WidgetsApi)
    EmptyContext
    (toHandler ctx)
    widgetsApiHandler
    req res

newApp :: Logger -> IO Application
newApp logger = do
  widgetsDb <- newTVarIO []
  pure $ app logger widgetsDb

