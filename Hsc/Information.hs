module Hsc.Information where

import Hsc.Construct

numaudiobuses    r = mkOsc r "NumAudioBuses"    [] 1 0 r0
numbuffers       r = mkOsc r "NumBuffers"       [] 1 0 r0
numcontrolbuses  r = mkOsc r "NumControlBuses"  [] 1 0 r0
numinputbuses    r = mkOsc r "NumInputBuses"    [] 1 0 r0
numoutputbuses   r = mkOsc r "NumOutputBuses"   [] 1 0 r0
numrunningsynths r = mkOsc r "NumRunningSynths" [] 1 0 r0
sampledur        r = mkOsc r "SampleDur"        [] 1 0 r0
samplerate       r = mkOsc r "SampleRate"       [] 1 0 r0
