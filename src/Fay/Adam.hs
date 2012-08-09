{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Adam where

import Language.Fay.FFI
import Language.Fay.Prelude

import Dom

main :: Fay ()
main = do
  printS "Adam.js loaded"
  addOnload onload

printS :: String -> Fay ()
printS = ffi "console.log(%1)"

onload :: Fay ()
onload = do
  printS "Adam.js onload"
  currentTime
--  firstByTag "body" >>= print
--  getWindow >>= print
--  getDocument >>= print
--  w <- getWindow
--  print [w,w]
--  form <- byId "f"
--  addEvent form "submit" (\e -> addLi e >> preventDefault e)
--  button <- byId "button"
--  addEvent button "click" addLi

--------------------------------------------------------------------------------
-- | Ajax for the current time splice.
--
--   $("#current-time-button").click(function () {
--     $.ajax("/ajax/current-time", {
--       success : function (resp) {
--         $("#current-time").html(resp.time);
--       }
--     });
--   });
currentTime :: Fay ()
currentTime = do
  button <- byId "current-time-button"
  addEvent button "click" sendRequest
    where
      sendRequest = ajaxJson "/ajax/current-time" handleResponse
      handleResponse json = do
                ctr <- jsonToCTR json
                el <- byId "current-time"
                setInnerHtml el (time ctr)
data Json
instance Foreign Json
data CurrentTimeResponse = CTR { time :: String }
instance Foreign CurrentTimeResponse
jsonToCTR :: Json -> Fay CurrentTimeResponse
jsonToCTR json = do t <- attrS json "time"
                    return CTR { time = t }
printctr :: CurrentTimeResponse -> Fay ()
printctr = ffi "console.log(%1)"
--------------------------------------------------------------------------------
ajaxJson :: String -> (Json -> Fay ()) -> Fay ()
ajaxJson = ffi "jQuery.ajax(%1, { success : %2 })"
attrS :: Foreign f => f -> String -> Fay String
attrS = ffi "adamHelpers.attr(%1,%2)"


addLi :: Event -> Fay Bool
addLi _ = do
  ul <- byId "ul"
  li <- createElement "li"
  setInnerHtml li "x"
  appendChild ul li
  return False
