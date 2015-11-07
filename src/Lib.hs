{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Lib
    ( ElementOptions
    , noKey
    , key
    , keyAndStatics
    , elementOpen
    , elementOpenStart
    , attr
    , elementOpenEnd
    , elementClose
    , element
    , elementVoid
    , text
    , patch
    , js_elementOpen
    , js_elementOpenStart
    , js_attr
    , js_elementOpenEnd
    , js_elementClose
    , js_elementVoid
    , js_text
    , js_patch
    ) where

import Control.Monad
import Data.JSString
import Data.Typeable
import GHC.Exts (Any)
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import GHCJS.DOM.Text
import GHCJS.Foreign ()
import GHCJS.Foreign.Callback
import GHCJS.Foreign.Export
import GHCJS.Marshal
import GHCJS.Prim (JSVal)
import GHCJS.Types
import JavaScript.Array
import Unsafe.Coerce

-- TODO flip this around
import Attributes hiding (Text)

{-
newtype ElementOpen a = ElementOpen (IO a)
newtype Incremental a = Incremental (IO a)

open :: String -> Maybe String -> ElementOpen () -> Incremental ()
incremental :: b -> (b -> Incremental a) -> IO a
-}

data ElementOptions
  = NoKey
  | Key JSString
  | KeyAndStatics JSString JSArray

noKey :: ElementOptions
noKey = NoKey

key :: JSString -> ElementOptions
key = Key

keyAndStatics :: JSString -> JSArray -> ElementOptions
keyAndStatics = KeyAndStatics

optVals :: ElementOptions -> (JSVal, JSVal)
optVals NoKey                = (nullRef, nullRef)
optVals (Key k)              = (jsval k, nullRef)
optVals (KeyAndStatics k ss) = (jsval k, jsval ss)

foreign import javascript unsafe "IncrementalDOM.elementOpen.apply(this, [$1, $2, $3].concat($4))" js_elementOpen :: JSString -> JSVal -> JSVal -> JSVal -> IO Element

attributesToArray :: [Attribute v e] -> IO JSArray
attributesToArray as = do
  arr <- create
  forM_ as $ \(Attribute k v) -> do
    push (jsval k) arr
    push v arr
  unsafeFreeze arr

elementOpen :: p e -> JSString -> ElementOptions -> [Attribute 'Valid e] -> IO Element
elementOpen _ name opts dynamics = do
  let (k, statics) = optVals opts
  js_elementOpen name k statics . jsval =<< attributesToArray dynamics

foreign import javascript unsafe "IncrementalDOM.elementOpenStart($1, $2, $3)" js_elementOpenStart :: JSString -> JSVal -> JSVal -> IO ()

elementOpenStart :: JSString -> ElementOptions -> IO ()
elementOpenStart name opts = do
  let (k, statics) = optVals opts
  js_elementOpenStart name k statics

foreign import javascript unsafe "IncrementalDOM.attr($1, $2)" js_attr :: JSString -> JSVal -> IO ()

attr :: ToJSVal a => JSString -> a -> IO ()
attr k v = js_attr k =<< toJSVal v

foreign import javascript unsafe "IncrementalDOM.elementOpenEnd()" js_elementOpenEnd :: IO Element

elementOpenEnd :: IO Element
elementOpenEnd = js_elementOpenEnd

foreign import javascript unsafe "IncrementalDOM.elementClose($1)" js_elementClose :: JSString -> IO Element

elementClose :: JSString -> IO Element
elementClose = js_elementClose

foreign import javascript unsafe "IncrementalDOM.elementVoid.apply(this, [$1, $2, $3].concat($4))" js_elementVoid :: JSString -> JSVal -> JSVal -> JSVal -> IO Element


elementVoid :: p e -> JSString -> ElementOptions -> [Attribute 'Valid e] -> IO Element
elementVoid _ name opts dynamics = do
  let (k, statics) = optVals opts
  js_elementVoid name k statics . jsval =<< attributesToArray dynamics


foreign import javascript unsafe "IncrementalDOM.text($1)" js_text :: JSString -> JSVal -> IO Text

-- | TODO figure out how to provide text transformations here if we want
text :: JSString -> IO Text
text str = js_text str nullRef

foreign import javascript unsafe "IncrementalDOM.patch($1, $2, $3)" js_patch :: Node -> Callback (JSVal -> IO ()) -> JSVal -> IO ()

patch :: Typeable a => Node -> a -> (a -> IO ()) -> IO ()
patch n x f = withExport x $ \xRef -> do
  cb <- syncCallback1 ThrowWouldBlock $ \ref -> do
    (Just xVal) <- derefExport $ unsafeCoerce ref
    f xVal
  js_patch n cb $ jsval xRef 
  releaseCallback cb

element :: p e -> JSString -> ElementOptions -> [Attribute 'Valid e] -> IO a -> IO (Element, a)
element p name opts dynamics inner = do
  e <- elementOpen p name opts dynamics
  a <- inner
  elementClose name
  return (e, a)

