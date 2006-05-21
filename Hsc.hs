module Hsc (module Hsc.List,
            module Hsc.U8v,
            module Hsc.Rate,
            module Hsc.UId,
            module Hsc.UGen,
            module Hsc.MCE,
            module Hsc.Construct,
            module Hsc.Graph,
            module Hsc.Dot,
            module Hsc.Math,
            module Hsc.Buffer,
            module Hsc.Demand,
            module Hsc.Envelope,
            module Hsc.Filter,
            module Hsc.FFT,
            module Hsc.Information,
            module Hsc.IO,
            module Hsc.Noise,
            module Hsc.Oscillator,
            module Hsc.Panner,
            module Hsc.OpenSoundControl,
            module Hsc.Server,
            module Hsc.Udp,
            module Hsc.Play,
            module Hsc.Schedule,
            module Hsc.Score,
            module Hsc.SndFile) where

import Hsc.List
import Hsc.U8v
import Hsc.Rate
import Hsc.UId
import Hsc.UGen
import Hsc.MCE
import Hsc.Construct
import Hsc.Graph
import Hsc.Dot
import Hsc.Math
import Hsc.Buffer
import Hsc.Demand
import Hsc.Envelope
import Hsc.Filter
import Hsc.FFT
import Hsc.Information
import Hsc.IO
import Hsc.Noise
import Hsc.Panner
import Hsc.Oscillator
import Hsc.OpenSoundControl
import Hsc.Server
import Hsc.Udp
import Hsc.Play
import Hsc.Schedule
import Hsc.Score
import Hsc.SndFile

-- analog bubbles

ab = out 0 $ combn s 0.2 0.2 4
  where s = sinosc AR (midicps f) 0 * 0.1
        f = lfsaw KR 0.4 0 * 24 + o
        o = lfsaw KR (MCE [8, 7.23]) 0 * 3 + 80


