module Hsc.Panner where

import Hsc.Construct (mkFilter)

linpan2 i x   level = mkFilter "LinPan2" [i,x,level]     2 0

pan2    i x   level = mkFilter "Pan2"    [i,x,level]     2 0
pan4    i x y level = mkFilter "Pan4"    [i,x,y,level]   4 0

panb  i az el level = mkFilter "PanB"    [i,az,el,level] 4 0
panb2 i az    level = mkFilter "PanB2"   [i,az,level]    3 0

rotate2 x y pos     = mkFilter "Rotate2" [x,y,pos]       2 0
