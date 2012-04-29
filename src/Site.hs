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
        , ("message", messageSplice)
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
messageSplice :: Splice AppHandler
messageSplice = do
    msgRef <- gets _message
    msg <- liftIO . readIORef $ msgRef
    return $ [TextNode $ T.decodeUtf8 msg]

messageHandler :: Handler App App ()
messageHandler = method GET getter <|> method POST setter
  where
    getter = do
        msgRef <- gets _message
        msg <- liftIO . readIORef $ msgRef
        writeBS msg
    setter = do
        newMsg <- getParam "msg"
        msgRef <- gets _message
        liftIO $ maybe (return ()) (writeIORef msgRef) newMsg
        index

messageHandler2 :: Handler App App ()
messageHandler2 = do
  msgRef <- gets _message
  msg <- liftIO . readIORef $ msgRef
  let msgT = T.decodeUtf8 msg

  (view, result) <- runForm "message" (messageForm (Just (Message msgT)))
  case result of
    Just (Message newMsg) -> do
        msgRef <- gets _message
        liftIO . writeIORef msgRef . T.encodeUtf8 $ newMsg
        index
    Nothing ->
        -- index
        heistLocal (bindDigestiveSplices view) $ render "message-form"

-- TODO Fix
--messageFormSplice :: Splice AppHandler
--messageFormSplice = do
--  (view, result) <- runForm "message" messageForm
--  heistLocal (bindDigestiveSplices view) $ render "message-form"


------------------------------------------------------------------------------
-- | renders the echo page.
echo :: Handler App App ()
echo = do
    message <- decodedParam "stuff"
    heistLocal (bindString "message" (T.decodeUtf8 message)) $ render "echo"
  where
    decodedParam p = fromMaybe "" <$> getParam p


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/", index)
         , ("/message2", messageHandler2)
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
    msg <- liftIO $ newIORef "Howdy"
    addRoutes routes
    return $ App { _heist = h, _startTime = sTime, _message = msg }


