module Web.RequestContext where

import Control.Concurrent.STM.TVar (TVar)
import Widgets (Widget)

newtype RequestContext = RequestContext
  { widgetsDb :: TVar [Widget]
  }

