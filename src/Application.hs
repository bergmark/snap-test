{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
--   handler monad.
--
module Application where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as B
import Data.IORef
import Data.Lens.Template
import Data.Time.Clock
import Snap.Snaplet
import Snap.Snaplet.Heist

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _startTime :: UTCTime
    , _message :: IORef B.ByteString
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
type AppHandler = Handler App App


