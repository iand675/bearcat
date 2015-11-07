{-# LANGUAGE OverloadedStrings #-}
module Main where

import Attributes
import Lib
import Html5
import Data.JSString
import GHCJS.DOM
import qualified GHCJS.DOM.Document as Doc
import qualified GHCJS.DOM.NodeList as NL
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.Foreign.Callback
import JavaScript.Array

main :: IO ()
main = do
  (Just doc) <- currentDocument
  (Just bodyTags) <- Doc.getElementsByClassName doc ("app" :: JSString)
  (Just body) <- NL.item bodyTags 0
  -- someFunc
    -- attrs <- mapM toJSVal_pure (["src", "https://www.google.com/images/branding/googlelogo/2x/googlelogo_color_272x92dp.png"] :: [JSString])
    -- varAttrs <- mapM toJSVal_pure ([] :: [JSString])
    -- _ <- js_elementVoid "img" "key" (fromList attrs) (fromList varAttrs)
  patch body (Right False :: Either String Bool) $ \x -> do
    div_ noKey [] $ do
      text ("Hello!" :: JSString)
      br_ noKey []
      text $ pack $ show x
      link_ noKey
        [ force $ rel_ "stylesheet"
        , force $ href_ "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha/css/bootstrap.min.css"
        ]
    return ()

