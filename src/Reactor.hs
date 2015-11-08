module Reactor where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Monad
import Data.Typeable
import GHCJS.DOM.Types (Node)
import Incremental

data Reactor a = Reactor
  { stateRef :: TMVar a
  , patcher  :: Patcher a
  , renderer :: Async ()
  }

reactor :: Typeable a => Node -> (a -> Incremental ()) -> IO (Reactor a)
reactor n f = do
  var <- newEmptyTMVarIO 
  p <- makePatcher f
  r <- async $ forever $ do
         x <- atomically $ takeTMVar var
         callPatcher n p x
  return $ Reactor var p r

cancelReactor :: Reactor a -> IO ()
cancelReactor r = do
  cancel $ renderer r
  releasePatcher $ patcher r

update :: Reactor a -> a -> IO ()
update r x = do
  atomically $ putTMVar (stateRef r) x
  -- Give reactor a chance to render immediately.
  -- Not sure if this is enough to totally *guarantee* that it will happen.
  yield
