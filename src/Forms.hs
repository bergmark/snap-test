{-# LANGUAGE OverloadedStrings #-}

module Forms where

import Control.Applicative
import Data.Text as T
import Text.Digestive
import Application

messageForm :: Monad m => Maybe Message -> Form Text m Message
messageForm m = Message <$> "message" .: check "Message length must be >= 4" checkMessageLength (text (_text <$> m))

checkMessageLength :: Text -> Bool
checkMessageLength = (>= 4) . T.length
