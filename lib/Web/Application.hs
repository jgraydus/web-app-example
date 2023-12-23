module Web.Application where

import Control.Concurrent.STM.TVar (newTVarIO, TVar)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Except (ExceptT)
import Servant
import Widgets (Widget(..))
import Web.RequestContext
import Web.Routes (WidgetsApi, widgetsApiHandler)

type RouteHandler = ReaderT RequestContext (ExceptT ServerError IO)

toHandler :: RequestContext -> RouteHandler a -> Handler a
toHandler ctx m = Handler (runReaderT m ctx)

app :: TVar [Widget] -> Application
app widgetsDb req res = do
  let ctx = RequestContext { widgetsDb }

  serveWithContextT
    (Proxy :: Proxy WidgetsApi)
    EmptyContext
    (toHandler ctx)
    widgetsApiHandler
    req res

newApp :: IO Application
newApp = do
  widgetsDb <- newTVarIO
    [ Widget { id = 42, name = "Large Widget" }
    , Widget { id = 17, name = "Small Widget" }
    ]
  pure $ app widgetsDb

