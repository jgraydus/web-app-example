module Web.RequestContext where

import Control.Concurrent.STM.TVar (TVar)
import Logger (Logger)
import Widgets (Widget)

data RequestContext = RequestContext
  { logger :: Logger
  , widgetsDb :: TVar [Widget]
  }

