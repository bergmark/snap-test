{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
--   site. The 'app' function is the initializer that combines everything
--   together and is exported by this module.
--
module Site
--  ( app
--  ) where
       where

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
------------------------------------------------------------------------------
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth hiding (session)
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Text.Digestive.Snap hiding (method)
import           Text.Digestive.Heist
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)
------------------------------------------------------------------------------
import           Application
import           Forms

-- | Notes
-- AppHandler = Handler App App
-- Splice m = HeistT m [Node]


------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: AppHandler ()
index = ifTop $ withSplices indexSnapletSplices $ heistLocal (bindSplices indexSplices) $ render "index"
    where
      ws :: HasHeist b => [(T.Text, SnapletSplice b v)] -> Handler b v a -> Handler b v a
      ws = withSplices

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
    return [TextNode . T.pack . show $ time]

------------------------------------------------------------------------------
-- | Splice for GET "foo" parameter
getFooSplice :: Splice AppHandler
getFooSplice = do
  foo <- getParam "foo"
  return [TextNode $ case foo of
                       Nothing -> "Nothing"
                       Just f -> T.decodeUtf8 f]

------------------------------------------------------------------------------
-- | The Message
getMessageRefMessage :: HeistT AppHandler (IORef (Maybe Message), Maybe Message)
getMessageRefMessage = do
  msgRef <- gets _message
  msg <- liftIO . readIORef $ msgRef
  return (msgRef, msg)

getMessageRefMessage2 :: AppHandler (IORef (Maybe Message), Maybe Message)
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

messageHandler :: AppHandler ()
messageHandler = do
  (msgRef, msg) <- getMessageRefMessage2

  (view, result) <- runForm "message" (messageForm msg)
  case result of
    Just newMsg -> writeJustRef msgRef newMsg >> index
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "message-form"

messageFormSplice :: Splice AppHandler
messageFormSplice = do
  (msgRef, msg) <- getMessageRefMessage
  (view, result) <- runForm "message" (messageForm msg)
  case result of
    Just newMsg -> writeJustRef msgRef newMsg >> splice view
    Nothing -> splice view

    where
      splice view = callTemplate "message-form" (digestiveSplices view)

------------------------------------------------------------------------------
-- | renders the echo page.
echo :: AppHandler ()
echo = do
    echoMsg <- decodedParam "stuff"
    heistLocal (bindString "message" (T.decodeUtf8 echoMsg)) $ render "echo"
  where
    decodedParam p = fromMaybe "" <$> getParam p

------------------------------------------------------------------------------
-- | Sessions
sessionSplice :: SnapletSplice App SessionManager
sessionSplice = do
  liftHandler (setInSession "x" "y")
  msg <- liftHandler sessionToList
  return $ [TextNode $ T.pack $ show $ msg]

sessionHandler :: Handler App SessionManager ()
sessionHandler = do
  msg <- sessionToList
  heistLocal (bindString "session-info" (T.pack $ show $ msg)) $ render "session"

------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (bindSplices errs) $ render "login"
  where
    errs = [("loginError", textSplice c) | c <- maybeToList authError]


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/",            index)

         , ("/login",       with auth handleLoginSubmit)
         , ("/logout",      with auth handleLogout)
         , ("/new_user",    with auth handleNewUser)

         , ("/message",     messageHandler)
         , ("/echo/:stuff", echo)
         , ("/session",     with session sessionHandler)

         , ("",             with heist heistServe)
         , ("",             serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | Splices
indexSplices :: [(T.Text, Splice AppHandler)]
indexSplices =
    [ ("start-time",   startTimeSplice)
    , ("current-time", currentTimeSplice)
    , ("message",      messageSplice)
    , ("message-form", messageFormSplice)
    , ("get-foo",      getFooSplice)
    ]

indexSnapletSplices :: [(T.Text, SnapletSplice App App)]
indexSnapletSplices = [("session-info", with session sessionSplice)]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    s <- nestSnaplet "sess" session $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings session "users.json"

    addAuthSplices auth
    addRoutes routes

    sTime <- liftIO getCurrentTime
    msg <- liftIO $ newIORef Nothing

    return $ App { _heist = h, _startTime = sTime, _message = msg, _session = s, _auth = a }
