module Hsc (module Hsc.List, 
            module Hsc.U8v,
            module Hsc.UGen,
            module Hsc.MCE,
            module Hsc.Graph,
            module Hsc.Dot,
            module Hsc.Math,
            module Hsc.Buffer,
            module Hsc.Envelope,
            module Hsc.Filter,
            module Hsc.FFT,                
            module Hsc.Information,
            module Hsc.IO,
            module Hsc.Noise,
            module Hsc.Oscillator,
            module Hsc.Panner,
            module Hsc.PV,
            module Hsc.OpenSoundControl,
            module Hsc.Server,
            module Hsc.Udp,
            module Hsc.Play) where

import Hsc.List
import Hsc.U8v
import Hsc.UGen
import Hsc.MCE
import Hsc.Graph
import Hsc.Dot
import Hsc.Math
import Hsc.Buffer
import Hsc.Envelope
import Hsc.Filter
import Hsc.FFT
import Hsc.Information
import Hsc.IO
import Hsc.Noise
import Hsc.Panner
import Hsc.PV
import Hsc.Oscillator
import Hsc.OpenSoundControl
import Hsc.Server
import Hsc.Udp
import Hsc.Play

-- analog bubbles

ab = out AR 0 $ combn AR s 0.2 0.2 4
  where s = sinosc AR (midicps f) 0 * 0.1
        f = lfsaw KR 0.4 0 * 24 + o
        o = lfsaw KR (MCE [8, 7.23]) 0 * 3 + 80


tg = out AR 0 $ tgrains 2 AR (impulse AR t 0) b 1 m d 0 0.1 2
    where b = 10
          m = mousex KR 0 (bufdur KR b) 0 0.2
          t = mousey KR 2 200 1 0.2
          d = 4 / t

pg f = out AR 0 $ sinosc AR f 0 * envgen KR 1 1 0 1 2 envperc' * 0.1

