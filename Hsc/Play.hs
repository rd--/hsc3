module Hsc.Play where

import Hsc.UGen
import Hsc.Graph
import Hsc.Math
import Hsc.Udp
import Hsc.Server
import Hsc.IO

playu fd u = do sync' fd "/done" (d_recv b)
                send' fd (s_new "Anonymous" (-1) 1 1)
    where g = graph u
          b = graphdef "Anonymous" g

hasOutputs :: UGen -> Bool
hasOutputs (UGen _ _ _ o _) = length o > 0
hasOutputs _                = False

play fd u 
    | hasOutputs u = playu fd (out AR 0 u)
    | otherwise    = playu fd u

play' sc u = do fd <- sc
                play fd u

stop fd = do send' fd (g_freeAll 1)

stop' sc = do fd <- sc
              send' fd (g_freeAll 1)
