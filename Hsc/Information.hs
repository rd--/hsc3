module Hsc.Information where

import Hsc.UGen

numaudiobuses    r = UGen r "NumAudioBuses"    [] [r] 0 r0
numbuffers       r = UGen r "NumBuffers"       [] [r] 0 r0
numcontrolbuses  r = UGen r "NumControlBuses"  [] [r] 0 r0
numinputbuses    r = UGen r "NumInputBuses"    [] [r] 0 r0
numoutputbuses   r = UGen r "NumOutputBuses"   [] [r] 0 r0
numrunningsynths r = UGen r "NumRunningSynths" [] [r] 0 r0
sampledur        r = UGen r "SampleDur"        [] [r] 0 r0
samplerate       r = UGen r "SampleRate"       [] [r] 0 r0
