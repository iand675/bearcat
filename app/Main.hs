{-# LANGUAGE OverloadedStrings #-}
module Main where

import Attributes
import Elements
import Control.Monad
import Incremental
import Data.JSString
import GHCJS.DOM
import GHCJS.DOM.Types (Element)
import qualified GHCJS.DOM.Document as Doc
import qualified GHCJS.DOM.NodeList as NL
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.Foreign.Callback
import JavaScript.Array
import Reactor

data TodoItem = TodoItem
  { todoId      :: Int
  , todoChecked :: Bool
  , todoText    :: JSString
  }

sampleTodos :: [TodoItem]
sampleTodos =
  [ TodoItem 0 False "Sell guitar"
  , TodoItem 1 False "Cancel extra credit cards"
  , TodoItem 2 False "Cancel phone service"
  , TodoItem 3 False "Get Yoshi's paperwork"
  , TodoItem 4 True "Get some omiyage"
  , TodoItem 5 True "Buy plane tickets"
  , TodoItem 6 True "Sell car"
  , TodoItem 7 True "Get car on craigslist"
  ]

renderTodo :: TodoItem -> Incremental Element
renderTodo x = fmap fst $ div_ (key $ pack $ show $ todoId x) [] $ do
  input_ noKey [type_ "checkbox", toggleable (todoChecked x) checked_]
  text $ todoText x

main :: IO ()
main = do
  (Just doc) <- currentDocument
  (Just bodyTags) <- Doc.getElementsByClassName doc ("app" :: JSString)
  (Just appContainer) <- NL.item bodyTags 0
  r <- reactor appContainer app
  update r $ Right False
  update r $ Left "wibble"
  cancelReactor r

app :: Either String Bool -> Incremental ()
app x = void $ do
  text "Hello!"
  br_ noKey []
  text $ pack $ show x
  div_ noKey [] $ text "Not Hidden"
  div_ noKey [hidden_] $ text "Hidden!"
  a_ noKey [href_ "http://iankduncan.com"] $ text "My homepage"
  mapM_ renderTodo sampleTodos
  pre_ noKey [] $ do
    text "module Main where\nimport Prelude\n"
  link_ noKey
    [ rel_ "stylesheet"
    , href_ "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha/css/bootstrap.min.css"
    ]

