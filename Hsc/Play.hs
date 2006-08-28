module Hsc.Play where

import Hsc.UGen (UGen(..))
import Hsc.Graph (graph, graphdef)
import Hsc.Udp (send', sync', close')
import Hsc.Server
import Hsc.UGens.IO (out)
import Control.Exception (bracket)

d_recv' n u = d_recv (graphdef n (graph u))

playu fd u = do r <- sync' fd (d_recv' "Anonymous" u)
                send' fd (s_new "Anonymous" (-1) AddToTail 1)
                return r

hasOutputs :: UGen -> Bool
hasOutputs (UGen _ _ _ o _ _) = not (null o)
hasOutputs (MCE _)            = True
hasOutputs _                  = False

play fd u
    | hasOutputs u = playu fd (out (Constant 0) u)
    | otherwise    = playu fd u

reset fd = do send' fd (g_freeAll 0)
              send' fd (g_new 1 AddToTail 0)

withfd fd' = bracket fd' close'

play'  sc u = withfd sc (\fd -> play fd u)
reset' sc   = withfd sc reset
