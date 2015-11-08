{-# LANGUAGE OverloadedStrings #-}
module Main where

import Attributes
import Elements
import Control.Monad
import Incremental
import Data.JSString
import GHCJS.DOM
import qualified GHCJS.DOM.Document as Doc
import qualified GHCJS.DOM.NodeList as NL
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.Foreign.Callback
import JavaScript.Array
import Reactor

main :: IO ()
main = do
  (Just doc) <- currentDocument
  (Just bodyTags) <- Doc.getElementsByClassName doc ("app" :: JSString)
  (Just body) <- NL.item bodyTags 0
  r <- reactor body app
  update r $ Right False
  update r $ Left "wibble"
  cancelReactor r

app :: Either String Bool -> Incremental ()
app x = void $ do
  text "Hello!"
  br_ noKey []
  text $ pack $ show x
  div_ noKey [toggleable False $ hidden_] $ text "Not Hidden"
  div_ noKey [toggleable True $ hidden_] $ text "Hidden!"
  a_ noKey [href_ "http://iankduncan.com"] $ text "My homepage"
  pre_ noKey [] $ do
    text "module Main where\nimport Prelude\n"
  link_ noKey
    [ rel_ "stylesheet"
    , href_ "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha/css/bootstrap.min.css"
    ]
  
