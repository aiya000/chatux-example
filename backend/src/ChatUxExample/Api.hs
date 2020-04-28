{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module ChatUxExample.Api where

import ChatUxExample.RIO
import Data.Aeson hiding (Result)
import Data.String.Here (i)
import Deriving.Aeson
import RIO hiding (logError, logDebug, logInfo, Handler, to)
import Servant

app :: Config -> Application
app x = serve api $ hoistServer api (runRIO x) server

type Api = "chat"
  :> QueryParam "text" Text
  :> Header "Origin" String
  :> Get '[JSON] Response

api :: Proxy Api
api = Proxy

data Response = ResponseSuccess Result
              | ResponseFailure Text -- ^ with the reason
  deriving stock (Show, Eq, Generic)

instance ToJSON Response where
  toJSON (ResponseSuccess result) = toJSON result
  toJSON (ResponseFailure reason) = toJSON reason

data Result = Result
  { output :: [Replying]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Replying for chatux
data Replying = Replying
  { replyingType :: String
  , replyingValue :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON) via CustomJSON '[FieldLabelModifier (StripPrefix "replying", CamelToSnake)] Replying


server :: ServerT Api (RIO Config)
server query origin = echoText query origin `catch` handleInternalError
  where
    handleInternalError :: SomeException -> RIO Config Response
    handleInternalError e = do
      let message = [i|Servere error! error: ${e}|]
      logError $ display message
      pure $ ResponseFailure message


echoText :: Maybe Text -> Maybe String -> RIO Config Response
echoText maybeText Nothing = do
  logInfo "A header 'Origin' not found. Regarding this access as a localhost access."
  echoText maybeText (Just "http://localhost")
echoText Nothing _ =
  throwString "a text is required as a URL query, but it doesn't specify."
echoText (Just text) (Just origin)
  | isKnownOrigin origin = pure . ResponseSuccess $ Result [Replying "text" text]
  | otherwise = pure $ ResponseFailure [i|An unknown origin '${origin}' is rejected.|]
  where
    isKnownOrigin "http://localhost" = True
    isKnownOrigin _ = False
