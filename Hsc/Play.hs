module Hsc.Play where

import Hsc.UGen
import Hsc.Graph
import Hsc.Math
import Hsc.Udp
import Hsc.Server
import Hsc.IO

playu fd u = do r <- sync' fd (d_recv b)
                send' fd (s_new "Anonymous" (-1) 1 1)
                return r
    where g = graph u
          b = graphdef "Anonymous" g

hasOutputs :: UGen -> Bool
hasOutputs (UGen _ _ _ o _ _) = length o > 0
hasOutputs (MCE _)            = True
hasOutputs _                  = False

play fd u 
    | hasOutputs u = playu fd (out AR 0 u)
    | otherwise    = playu fd u

init_ fd = do send' fd (g_new 1 1 0)
stop  fd = do send' fd (g_freeAll 1)

withfd fd' a = do fd <- fd'
                  a fd
                  close' fd

play' sc u = withfd sc (\fd -> play fd u)
stop' sc   = withfd sc stop
init' sc   = withfd sc init_
