module Web.Application where

import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, responseLBS)

app :: Application
app req res = res $ responseLBS status200 [] "Hello!"

