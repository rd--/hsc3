module Hsc.Play where

import Hsc.UGen (UGen(..))
import Hsc.Graph (graph, graphdef)
import Hsc.Udp (send', sync', close')
import Hsc.Server
import Hsc.UGens.IO (out)
import Control.Exception (bracket)

d_recv' n u = d_recv (graphdef n (graph u))

hasOutputs :: UGen -> Bool
hasOutputs (UGen _ _ _ o _ _) = not (null o)
hasOutputs (MCE _)            = True
hasOutputs _                  = False

addOut :: UGen -> UGen
addOut u = if hasOutputs u then out (Constant 0) u else u

init_ fd   = send' fd (g_new 1 AddToTail 0)
play  fd u = do r <- sync' fd (d_recv' "Anonymous" (addOut u))
                send' fd (s_new "Anonymous" (-1) AddToTail 1)
                return r
stop  fd   = send' fd (g_freeAll 1)
reset fd   = do send' fd (g_freeAll 0)
                init_ fd

withfd fd' = bracket fd' close'

init'  sc   = withfd sc init_
play'  sc u = withfd sc (\fd -> play fd u)
stop'  sc   = withfd sc stop
reset' sc   = withfd sc reset
