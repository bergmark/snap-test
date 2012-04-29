{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
--   handler monad.
--
module Application where

------------------------------------------------------------------------------
import Data.IORef
import Data.Lens.Template
import Data.Text
import Data.Time.Clock

import Snap.Snaplet
import Snap.Snaplet.Heist

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _startTime :: UTCTime
    , _message :: IORef (Maybe Message)
    }


-- <jaspervdj> Text for unicode text (e.g. human-readable) and ByteString for binary data or text with an unknown encoding
data Message = Message
               { _text :: Text }
               deriving Show

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
type AppHandler = Handler App App


