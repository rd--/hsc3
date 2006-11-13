module Sound.SC3.Server.Play where

import Sound.SC3.UGen.UGen (UGen(..))
import Sound.SC3.UGen.IO (out)
import Sound.SC3.UGen.Graph (graph)
import Sound.SC3.Server.Graphdef (graphdef)
import Sound.SC3.Server.Udp (send', sync', close')
import Sound.SC3.Server.Command (AddAction(AddToTail), s_new, d_recv, g_new, g_freeAll)
import Sound.SC3.Server.OpenSoundControl (OSC)

import Network.Socket (Socket)
import Control.Exception (bracket)

d_recv' :: String -> UGen -> OSC
d_recv' n u = d_recv (graphdef n (graph u))

hasOutputs :: UGen -> Bool
hasOutputs (UGen _ _ _ o _ _) = not (null o)
hasOutputs (MCE _)            = True
hasOutputs _                  = False

addOut :: UGen -> UGen
addOut u = if hasOutputs u then out (Constant 0) u else u

init_ :: Socket -> IO Int
init_ fd = send' fd (g_new 1 AddToTail 0)

play :: Socket -> UGen -> IO OSC
play  fd u = do r <- sync' fd (d_recv' "Anonymous" (addOut u))
                send' fd (s_new "Anonymous" (-1) AddToTail 1)
                return r

stop :: Socket -> IO Int
stop  fd = send' fd (g_freeAll 1)

reset :: Socket -> IO Int
reset fd = do send' fd (g_freeAll 0)
              init_ fd

withfd :: IO Socket -> (Socket -> IO a) -> IO a
withfd fd' = bracket fd' close'

init' :: IO Socket -> IO Int
init'  sc   = withfd sc init_

play' :: IO Socket -> UGen -> IO OSC
play'  sc u = withfd sc (flip play u)

stop' :: IO Socket -> IO Int
stop'  sc   = withfd sc stop

reset' :: IO Socket -> IO Int
reset' sc   = withfd sc reset
