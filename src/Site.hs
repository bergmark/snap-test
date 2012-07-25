{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
--   site. The 'app' function is the initializer that combines everything
--   together and is exported by this module.
--
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Data.IORef
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Text.Digestive.Snap hiding (method)
import           Text.Digestive.Heist
import qualified Text.Digestive.View
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)
------------------------------------------------------------------------------
import           Application
import           Forms


------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Handler App App ()
index = ifTop $ heistLocal (bindSplices indexSplices) $ render "index"
  where
    indexSplices =
        [ ("start-time",   startTimeSplice)
        , ("current-time", currentTimeSplice)
        , ("message",      messageSplice)
        , ("message-form", messageFormSplice)
        ]


------------------------------------------------------------------------------
-- | For your convenience, a splice which shows the start time.
startTimeSplice :: Splice AppHandler
startTimeSplice = do
    time <- lift $ gets _startTime
    return $ [TextNode $ T.pack $ show $ time]


------------------------------------------------------------------------------
-- | For your convenience, a splice which shows the current time.
currentTimeSplice :: Splice AppHandler
currentTimeSplice = do
    time <- liftIO getCurrentTime
    return $ [TextNode $ T.pack $ show $ time]

------------------------------------------------------------------------------
-- | The Message

getMessageRefMessage :: HeistT AppHandler (IORef (Maybe Message), Maybe Message)
getMessageRefMessage = do
  msgRef <- gets _message
  msg <- liftIO . readIORef $ msgRef
  return (msgRef, msg)

getMessageRefMessage2 :: Handler App App (IORef (Maybe Message), Maybe Message)
getMessageRefMessage2 = do
  msgRef <- gets _message
  msg <- liftIO . readIORef $ msgRef
  return (msgRef, msg)

writeJustRef :: MonadIO m => IORef (Maybe a) -> a -> m ()
writeJustRef ref v = liftIO $ writeIORef ref (Just v)

messageSplice :: Splice AppHandler
messageSplice = do
    (_, msg) <- getMessageRefMessage
    let msgText = maybe "N/A" _text msg
    return [TextNode msgText]

messageHandler :: Handler App App ()
messageHandler = do
  (msgRef, msg) <- getMessageRefMessage2

  (view, result) <- runForm "message" (messageForm msg)
  case result of
    Just newMsg -> writeJustRef msgRef newMsg >> index
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "message-form"

messageFormSplice :: Splice AppHandler
messageFormSplice = do
  (msgRef, msg) <- getMessageRefMessage
  (view :: Text.Digestive.View.View T.Text, result :: Maybe Message) <- runForm "message" (messageForm msg)
  case result of
    Just newMsg -> writeJustRef msgRef newMsg >> splice view
    Nothing -> splice view

    where
      splice view = callTemplate "message-form" (digestiveSplices view)

------------------------------------------------------------------------------
-- | renders the echo page.
echo :: Handler App App ()
echo = do
    echoMsg <- decodedParam "stuff"
    heistLocal (bindString "message" (T.decodeUtf8 echoMsg)) $ render "echo"
  where
    decodedParam p = fromMaybe "" <$> getParam p


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/", index)
         , ("/message", messageHandler)
         , ("/echo/:stuff", echo)
         , ("", with heist heistServe)
         , ("", serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    sTime <- liftIO getCurrentTime
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    addRoutes routes
    msg <- liftIO $ newIORef Nothing
    return $ App { _heist = h, _startTime = sTime, _message = msg }
