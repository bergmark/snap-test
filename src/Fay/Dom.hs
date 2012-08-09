{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Dom where

data Element
instance Foreign Element
data Event
instance Foreign Event
data Global
instance Foreign Global
data Document
instance Foreign Document

print :: Foreign a => a -> Fay ()
print = ffi "console.log(%1)"

head :: [a] -> a
head (x:_) = x

getBody :: Fay Element
getBody = firstByTag "body"

getWindow :: Fay Global
getWindow = ffi "adamHelpers.getWindow()"

getDocument :: Fay Document
getDocument = ffi "adamHelpers.getDocument()"

firstByTag :: String -> Fay Element
firstByTag tag = byTag tag >>= (return . head)

byTag :: String -> Fay [Element]
byTag = ffi "document.getElementsByTagName(%1)"

byId :: String -> Fay Element
byId = ffi "document.getElementById(%1)"

addEvent :: Foreign f => Element -> String -> (Event -> Fay f) -> Fay ()
addEvent = ffi "%1.addEventListener(%2,%3)"

addOnload :: Foreign f => Fay f -> Fay ()
addOnload = ffi "adamHelpers.addOnload(%1)"

stopProp :: Event -> Fay ()
stopProp = ffi "%1.stopPropagation()"

preventDefault :: Event -> Fay ()
preventDefault = ffi "%1.preventDefault()"

createElement :: String -> Fay Element
createElement = ffi "document.createElement(%1)"

setInnerHtml :: Element -> String -> Fay ()
setInnerHtml = ffi "adamHelpers.setInnerHtml(%1, %2)"

appendChild :: Element -> Element -> Fay ()
appendChild = ffi "%1.appendChild(%2)"

--selFirst :: String -> String -> Fay Element
--selFirst func s = sel func s >>= (return . head)

--sel :: String -> String -> Fay [Element]
--sel func = ffi func ""
