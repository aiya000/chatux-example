module ChatUxExample.Main where

import qualified ChatUxExample.Api as Api
import ChatUxExample.RIO (Config, logInfo)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsRequestHeaders)
import Network.Wai.Middleware.Servant.Options (provideOptions)
import RIO hiding (logInfo)

-- | The standard entry point
app :: RIO Config ()
app = do
  logInfo "Listening on port 25252"
  config <- ask
  liftIO $ running config
  where
    running config = Warp.run 25252
      $ cors (const $ Just policy)
      $ provideOptions Api.api
      $ Api.app config

    policy = simpleCorsResourcePolicy
      { corsRequestHeaders = [ "Content-Type" ]
      }
