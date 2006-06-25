module Hsc.Information where

import Hsc.Construct (mkOsc)

numaudiobuses    r = mkOsc r "NumAudioBuses"    [] 1 0
numbuffers       r = mkOsc r "NumBuffers"       [] 1 0
numcontrolbuses  r = mkOsc r "NumControlBuses"  [] 1 0
numinputbuses    r = mkOsc r "NumInputBuses"    [] 1 0
numoutputbuses   r = mkOsc r "NumOutputBuses"   [] 1 0
numrunningsynths r = mkOsc r "NumRunningSynths" [] 1 0
sampledur        r = mkOsc r "SampleDur"        [] 1 0
samplerate       r = mkOsc r "SampleRate"       [] 1 0
