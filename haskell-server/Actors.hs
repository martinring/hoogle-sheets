module Actors where

import Control.Concurrent

data Message a = Stop | Message {
  messageSender :: (ActorRef a),
  payload :: a
}

newtype ActorRef a = ActorRef {
  inbox :: MVar (Message a)
}

data ActorContext a = ActorContext {
  self :: ActorRef a,
  sender :: ActorRef a
}

newtype Behavior a = Behavior {
  receive :: ActorContext a -> a -> IO (Behavior a)
}

respond :: ActorContext a -> a -> IO ()
respond context msg =
  send (sender context) (self context) msg

send :: ActorRef a -> ActorRef a -> a -> IO ()
send (ActorRef recipient) sender message =
  putMVar recipient (Message sender message)

ask :: ActorRef a -> a -> IO a
ask recipient message = do
  inbox <- newEmptyMVar
  let self = ActorRef inbox
  send recipient self message
  (Message sender answer) <- takeMVar inbox
  return answer

stop :: ActorRef a -> IO ()
stop (ActorRef recipient) = putMVar recipient Stop

become :: (ActorContext a -> a -> IO (Behavior a)) -> IO (Behavior a)
become = return . Behavior

actor :: Behavior a -> IO (ActorRef a)
actor behavior = do
  inbox <- newEmptyMVar
  let self = ActorRef inbox
  let loop (Behavior behavior) = do
        msg <- takeMVar inbox
        case msg of
          Stop -> return ()
          (Message sender msg) -> do
            let context = ActorContext self sender
            newState <- behavior context msg
            loop newState
  forkIO $ loop behavior
  return self