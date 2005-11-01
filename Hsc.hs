module Hsc (module Hsc.List, 
            module Hsc.U8v,
            module Hsc.UGen,
            module Hsc.MCE,
            module Hsc.Graph,
            module Hsc.Math,
            module Hsc.Oscillator,
            module Hsc.Filter,
            module Hsc.IO,
            module Hsc.OpenSoundControl,
            module Hsc.Server,
            module Hsc.Udp,
            module Hsc.Play) where

import Hsc.List
import Hsc.U8v
import Hsc.UGen
import Hsc.MCE
import Hsc.Graph
import Hsc.Math
import Hsc.Oscillator
import Hsc.Filter
import Hsc.IO
import Hsc.OpenSoundControl
import Hsc.Server
import Hsc.Udp
import Hsc.Play

-- analog bubbles

ab = out AR 0 (combn AR s 0.2 0.2 4)
  where s = sinosc AR (midicps f) 0 * 0.1
        f = lfsaw KR 0.4 0 * 24 + o
        o = lfsaw KR (MCE [8, 7.23]) 0 * 3 + 80

