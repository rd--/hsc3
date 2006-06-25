module Hsc.Play where

import Hsc.UGen (UGen(..))
import Hsc.Graph (graph, graphdef)
import Hsc.Udp (send', sync', close')
import Hsc.Server
import Hsc.IO (out)
import Hsc.OpenSoundControl (Osc)

import Network.Socket (Socket)

d_recv' n u = d_recv (graphdef n (graph u))

playu fd u = do r <- sync' fd (d_recv' "Anonymous" u)
                send' fd (s_new "Anonymous" (-1) AddToTail 1)
                return r

hasOutputs :: UGen -> Bool
hasOutputs (UGen _ _ _ o _ _) = length o > 0
hasOutputs (MCE _)            = True
hasOutputs _                  = False

play fd u
    | hasOutputs u = playu fd (out (Constant 0) u)
    | otherwise    = playu fd u

init_ fd = do send' fd (g_new 1 AddToTail 0)
stop  fd = do send' fd (g_freeAll 1)

withfd fd' a = do fd <- fd'
                  a fd
                  close' fd

play' sc u = withfd sc (\fd -> play fd u)
stop' sc   = withfd sc stop
init' sc   = withfd sc init_
