{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Dom where

import Language.Fay.FFI
import Language.Fay.Prelude

data Element
instance Foreign Element
data Event
instance Foreign Event
data Global
instance Foreign Global
data Document
instance Foreign Document

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" ""

head :: [a] -> a
head (x:_) = x

getBody :: Fay Element
getBody = firstByTag "body"

getWindow :: Fay Global
getWindow = foreignFay "adamHelpers.getWindow" "Global"

getDocument :: Fay Document
getDocument = foreignFay "adamHelpers.getDocument" "Document"

firstByTag :: String -> Fay Element
firstByTag tag = byTag tag >>= (return . head)

byTag :: String -> Fay [Element]
byTag = foreignFay "document.getElementsByTagName" "array"

byId :: String -> Fay Element
byId = foreignFay "document.getElementById" "Element"

addEvent :: Foreign f => Element -> String -> (Event -> Fay f) -> Fay ()
addEvent = foreignMethodFay "addEventListener" ""

addOnload :: Foreign f => Fay f -> Fay ()
addOnload = foreignFay "adamHelpers.addOnload" ""

stopProp :: Event -> Fay ()
stopProp = foreignMethodFay "stopPropagation" ""

preventDefault :: Event -> Fay ()
preventDefault = foreignMethodFay "preventDefault" ""

createElement :: String -> Fay Element
createElement = foreignFay "document.createElement" "Element"

setInnerHtml :: Element -> String -> Fay ()
setInnerHtml = foreignFay "adamHelpers.setInnerHtml" ""

appendChild :: Element -> Element -> Fay ()
appendChild = foreignMethodFay "appendChild" ""

selFirst :: String -> String -> Fay Element
selFirst func s = sel func s >>= (return . head)

sel :: String -> String -> Fay [Element]
sel func = foreignFay func ""
