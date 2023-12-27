module Web.Application where

import Control.Concurrent.STM.TVar (newTVarIO, TVar)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Except (ExceptT)
import Data.UUID (toText)
import Data.UUID.V4 qualified as UUID
import Logger (LogLevel(INFO), Logger(..), toLogStr, withPrefix)
import Servant
import System.Clock
  (diffTimeSpec, getTime, Clock(Monotonic), TimeSpec, toNanoSecs)
import Widgets (Widget(..))
import Web.RequestContext
import Web.Routes (WidgetsApi, widgetsApiHandler)

type RouteHandler = ReaderT RequestContext (ExceptT ServerError IO)

toHandler :: RequestContext -> RouteHandler a -> Handler a
toHandler ctx m = Handler (runReaderT m ctx)

app :: Logger -> TVar [Widget] -> Application
app logger' widgetsDb req res = do
  startTime <- getTime Monotonic
  requestId <- UUID.nextRandom

  let reqIdStr = "[request id: " <> toLogStr (toText requestId) <> "]"
      logger@Logger { runLogger } = withPrefix reqIdStr logger'
      ctx = RequestContext { logger, widgetsDb }

  runLogger INFO (toLogStr $ show req)

  result <- serveWithContextT
              (Proxy :: Proxy WidgetsApi)
              EmptyContext
              (toHandler ctx)
              widgetsApiHandler
              req res

  endTime <- getTime Monotonic
  let diff = msDiff startTime endTime
  runLogger INFO $ "request took: " <> toLogStr diff <> " ms"

  pure result

msDiff :: TimeSpec -> TimeSpec -> Double
msDiff start end =
  let diff = diffTimeSpec start end
  in (fromIntegral $ toNanoSecs diff) / 1000000

newApp :: Logger -> IO Application
newApp logger = do
  widgetsDb <- newTVarIO []
  pure $ app logger widgetsDb

