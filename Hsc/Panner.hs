module Hsc.Panner where

import Hsc.UGen

linpan2 r i x   level = proxyU r "LinPan2" [i,x,level]     [r,r]     0 0

pan2    r i x   level = proxyU r "Pan2"    [i,x,level]     [r,r]     0 0
pan4    r i x y level = proxyU r "Pan4"    [i,x,y,level]   [r,r,r,r] 0 0

panb  r i az el level = proxyU r "PanB"    [i,az,el,level] [r,r,r,r] 0 0
panb2 r i az    level = proxyU r "PanB2"   [i,az,level]    [r,r,r]   0 0
