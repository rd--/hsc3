{- | Module to provide a stateful connection to scsynth.

The purpose is to store Osc Messages that should be sent when resetting the synthesiser.

This should, but does not:

- allow for Scsynth to be at a non-standard address
- allow for multiple Scsynth instances
-}
module Sound.Sc3.Server.Scsynth where

import Data.IORef {- base -}

import qualified Sound.Osc.Packet as Osc {- hosc -}

import Sound.Sc3.Ugen.Ugen {- hsc3 -}
import Sound.Sc3.Ugen.Util {- hsc3 -}

import Sound.Sc3.Server.Transport.Monad {- hsc3 -}

-- | Scsynth state.
data Scsynth = Scsynth {scsynthResetMessages :: IORef [Osc.Message]}

-- | Scsynth with no messages or allocated buffers.
newScsynth :: IO Scsynth
newScsynth = fmap Scsynth (newIORef [])

-- | Print onReset messages.
scsynthPrint :: Scsynth -> IO ()
scsynthPrint (Scsynth mRef) = do
  m <- readIORef mRef
  print m

{- | Add a sequence of messages to be sent on scsynth reset.

> scsynth <- newScsynth
> scsynthOnReset scsynth [b_free 100]
> scsynthPrint scsynth
-}
scsynthOnReset :: Scsynth -> [Osc.Message] -> IO ()
scsynthOnReset (Scsynth mRef) messages =
  if not (null messages)
    then modifyIORef' mRef (++ messages)
    else return ()

-- | reset scsynth, send all stored onReset messages, clear the onReset message store.
scsynthReset :: Scsynth -> IO ()
scsynthReset (Scsynth mRef) = do
  onResetMessages <- readIORef mRef
  writeIORef mRef []
  withSc3 (reset >> mapM_ maybe_async onResetMessages)

{- | Play Ugen at Scsynth.
Send any required initialisation messages and stores and onReset messages.
-}
scsynthPlayAt :: Scsynth -> Play_Opt -> Ugen -> IO ()
scsynthPlayAt scsynth opt ugen = do
  let (pre, post) = ugenCollectBrackets ugen
  if not (null post) then scsynthOnReset scsynth post else return ()
  withSc3 (mapM_ maybe_async pre >> playAt opt ugen)

-- | scsynthPlayAt with default options.
scsynthPlay :: Scsynth -> Ugen -> IO ()
scsynthPlay scsynth = scsynthPlayAt scsynth def_play_opt
