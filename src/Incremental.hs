{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Incremental
    ( ElementOptions
    , Incremental
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
    , Patcher
    , makePatcher
    , callPatcher
    , releasePatcher
    , notifications
    , onNodesCreated
    , onNodesDeleted
    , js_elementOpen
    , js_elementOpenStart
    , js_attr
    , js_elementOpenEnd
    , js_elementClose
    , js_elementVoid
    , js_text
    , js_patch
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans
import Data.JSString
import Data.String
import Data.Typeable
import GHC.TypeLits
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import GHCJS.DOM.Text
import GHCJS.Foreign ()
import GHCJS.Foreign.Callback
import GHCJS.Foreign.Callback.Internal (Callback(..))
import GHCJS.Foreign.Export
import GHCJS.Marshal
import GHCJS.Prim (JSVal)
import GHCJS.Types
import JavaScript.Array
import JavaScript.Object hiding (create)
import JavaScript.Object.Internal (Object(..))
import Unsafe.Coerce

-- TODO flip this around
import Attributes hiding (Text)

{-
newtype ElementOpen a = ElementOpen (IO a)
-}

newtype Incremental a = Incremental (IO a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance MonadBase Incremental Incremental where
  liftBase = id

instance IsString (Incremental Text) where
  fromString = text . pack

{-
open :: String -> Maybe String -> ElementOpen () -> Incremental ()
-}

newtype StaticAttributes (e :: Symbol) = StaticAttributes JSArray

incremental :: Incremental a -> IO a
incremental (Incremental m) = m

data ElementOptions (e :: Symbol)
  = NoKey
  | Key {-# UNPACK #-} !JSString
  | KeyAndStatics {-# UNPACK #-} !JSString
                  {-# UNPACK #-} !(StaticAttributes e)

noKey :: Monad m => m (ElementOptions e)
noKey = return NoKey

key :: Monad m => JSString -> m (ElementOptions e)
key = return . Key

keyAndStatics :: Monad m => JSString -> StaticAttributes e -> m (ElementOptions e)
keyAndStatics str = return . KeyAndStatics str

optVals :: ElementOptions e -> (JSVal, JSVal)
optVals NoKey                = (nullRef, nullRef)
optVals (Key k)              = (jsval k, nullRef)
optVals (KeyAndStatics k (StaticAttributes ss)) = (jsval k, jsval ss)

foreign import javascript unsafe "IncrementalDOM.elementOpen.apply(this, [$1, $2, $3].concat($4))" js_elementOpen :: JSString -> JSVal -> JSVal -> JSVal -> IO Element

attributesToArray :: [Attribute e] -> IO JSArray
attributesToArray as = do
  arr <- create
  forM_ as $ \(Attribute k v) -> do
    push (jsval k) arr
    push v arr
  unsafeFreeze arr

elementOpen :: MonadBase Incremental m => p e -> JSString -> m (ElementOptions e) -> [Attribute e] -> m Element
elementOpen _ name opts dynamics = do
  (k, statics) <- optVals <$> opts
  liftBase $ Incremental (js_elementOpen name k statics . jsval =<< attributesToArray dynamics)

foreign import javascript unsafe "IncrementalDOM.elementOpenStart($1, $2, $3)" js_elementOpenStart :: JSString -> JSVal -> JSVal -> IO ()

elementOpenStart :: JSString -> ElementOptions e -> IO ()
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

elementClose :: MonadBase Incremental m => JSString -> m Element
elementClose = liftBase . Incremental . js_elementClose

foreign import javascript unsafe "IncrementalDOM.elementVoid.apply(this, [$1, $2, $3].concat($4))" js_elementVoid :: JSString -> JSVal -> JSVal -> JSVal -> IO Element


elementVoid :: MonadBase Incremental m => p e -> JSString -> m (ElementOptions e) -> [Attribute e] -> m Element
elementVoid _ name opts dynamics = do
  (k, statics) <- optVals <$> opts
  liftBase $ Incremental (js_elementVoid name k statics . jsval =<< attributesToArray dynamics)

foreign import javascript unsafe "IncrementalDOM.text($1)" js_text :: JSString -> JSVal -> IO Text

-- | TODO figure out how to provide text transformations here if we want
text :: MonadBase Incremental m => JSString -> m Text
text str = liftBase $ Incremental (js_text str nullRef)

foreign import javascript unsafe "IncrementalDOM.patch($1, $2, $3)" js_patch :: Node -> Callback (JSVal -> IO ()) -> JSVal -> IO ()

patch :: Typeable a => Node -> (a -> Incremental ()) -> a -> IO ()
patch n f x = do
  -- TODO replace with bracket
  p <- makePatcher f
  callPatcher n p x
  releasePatcher p

newtype Patcher a = Patcher (Callback (JSVal -> IO ()))

makePatcher :: Typeable a => (a -> Incremental ()) -> IO (Patcher a)
makePatcher f = do
  cb <- syncCallback1 ThrowWouldBlock $ \ref -> do
    (Just xVal) <- derefExport $ unsafeCoerce ref
    let (Incremental m) = f xVal
    m
  return $ Patcher cb

callPatcher :: Typeable a => Node -> Patcher a -> a -> IO ()
callPatcher n (Patcher cb) x = withExport x (js_patch n cb . jsval)

releasePatcher :: Patcher a -> IO ()
releasePatcher (Patcher f) = releaseCallback f

element :: MonadBase Incremental m => p e -> JSString -> m (ElementOptions e) -> [Attribute e] -> m a -> m (Element, a)
element p name opts dynamics inner = do
  e <- elementOpen p name opts dynamics
  a <- inner
  elementClose name
  return (e, a)

foreign import javascript unsafe "IncrementalDOM.notifications" notifications :: Object

onNodesCreated :: Maybe ([Node] -> IO ()) -> IO ()
onNodesCreated mf = do
  val <- unsafeGetProp "nodesCreated" notifications 
  if isNull val
    then return ()
    else do
      isHsFun <- do
        prop <- unsafeGetProp "isHsCallback" (Object val)
        if isUndefined prop
          then return False
          else fromJSValUnchecked prop
      if isHsFun
        then releaseCallback (unsafeCoerce val :: Callback (JSVal -> IO ()))
        else return ()
  case mf of
    Nothing -> unsafeSetProp "nodesCreated" nullRef notifications
    Just f -> do
      (Callback cb) <- syncCallback1 ContinueAsync $ \arr -> do
        ns <- fromJSValUnchecked arr
        f ns
      yes <- toJSVal True
      unsafeSetProp "isHsCallback" yes (Object cb) 
      unsafeSetProp "nodesCreated" cb notifications 

onNodesDeleted :: Maybe ([Node] -> IO ()) -> IO ()
onNodesDeleted mf = do
  val <- unsafeGetProp "nodesDeleted" notifications 
  if isNull val
    then return ()
    else do
      isHsFun <- do
        prop <- unsafeGetProp "isHsCallback" (Object val)
        if isUndefined prop
          then return False
          else fromJSValUnchecked prop
      if isHsFun
        then releaseCallback (unsafeCoerce val :: Callback (JSVal -> IO ()))
        else return ()
  case mf of
    Nothing -> unsafeSetProp "nodesDeleted" nullRef notifications
    Just f -> do
      (Callback cb) <- syncCallback1 ContinueAsync $ \arr -> do
        ns <- fromJSValUnchecked arr
        f ns
      yes <- toJSVal True
      unsafeSetProp "isHsCallback" yes (Object cb) 
      unsafeSetProp "nodesDeleted" cb notifications 
