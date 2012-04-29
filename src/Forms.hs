{-# LANGUAGE OverloadedStrings #-}

module Forms where

import Control.Applicative
import Data.Text as T
import Text.Digestive

data Message = Message
               { message :: Text }
               deriving Show

messageForm :: Monad m => Maybe Message -> Form Text m Message
messageForm m = Message
    <$> "message" .: check "Message length must be >= 4" checkMessageLength (text (Just (maybe "empty" message m)))

checkMessageLength :: Text -> Bool
checkMessageLength = (>= 4) . T.length
