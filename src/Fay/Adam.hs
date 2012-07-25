{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Adam (addLi) where

import Language.Fay.FFI
import Language.Fay.Prelude

import Dom

main :: Fay ()
main = addOnload onload

onload :: Fay ()
onload = do
  firstByTag "body" >>= print
  getWindow >>= print
  getDocument >>= print
  w <- getWindow
  print [w,w]
  form <- byId "f"
  addEvent form "submit" (\e -> addLi e >> preventDefault e)
  button <- byId "button"
  addEvent button "click" addLi

addLi :: Event -> Fay Bool
addLi _ = do
  ul <- byId "ul"
  li <- createElement "li"
  setInnerHtml li "x"
  appendChild ul li
  return False
