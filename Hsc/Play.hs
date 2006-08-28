module Hsc.Play where

import Hsc.UGen (UGen(..))
import Hsc.Graph (graph, graphdef)
import Hsc.Udp (send', sync', close')
import Hsc.Server
import Hsc.UGens.IO (out)
import Control.Exception (bracket)

d_recv' n u = d_recv (graphdef n (graph u))

noId :: Int
noId = -1

play fd sid u =
   do r <- sync' fd (d_recv' "Anonymous" u)
      send' fd (s_new "Anonymous" sid AddToTail 1)
      return r

hasOutputs :: UGen -> Bool
hasOutputs (UGen _ _ _ o _ _) = not (null o)
hasOutputs (MCE _)            = True
hasOutputs _                  = False

addOut :: UGen -> UGen
addOut u =
   if hasOutputs u
     then (out (Constant 0) u)
     else u

init_ fd = send' fd (g_new 1 AddToTail 0)
stop  fd = send' fd (g_freeAll 1)
stopSingle fd sid = send' fd (n_free sid)
reset fd = do send' fd (g_freeAll 0)
              init_ fd

withfd fd' = bracket fd' close'

init'  sc   = withfd sc init_
play'  sc u = withfd sc (\fd -> play fd noId (addOut u))
stop'  sc   = withfd sc stop
reset' sc   = withfd sc reset
set' sc name value = withfd sc (\fd -> send' fd (n_set noId name value))
setMulti' sc attrs = withfd sc (\fd -> send' fd (n_set' noId attrs))

playId sc sid u = withfd sc (\fd -> play fd sid (addOut u))
stopId sc sid   = withfd sc (\fd -> stopSingle fd sid)
setId  sc sid name value = withfd sc (\fd -> send' fd (n_set sid name value))
