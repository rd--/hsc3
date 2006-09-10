module Sound.SC3.UGen.Information where

import Sound.SC3.UGen.Rate (Rate)
import Sound.SC3.UGen.UGen (UGen, mkOsc)

numAudioBuses    r = mkOsc r "NumAudioBuses"    [] 1 0
numBuffers       r = mkOsc r "NumBuffers"       [] 1 0
numControlBuses  r = mkOsc r "NumControlBuses"  [] 1 0
numInputBuses    r = mkOsc r "NumInputBuses"    [] 1 0
numOutputBuses   r = mkOsc r "NumOutputBuses"   [] 1 0
numRunningSynths r = mkOsc r "NumRunningSynths" [] 1 0
sampleDur        r = mkOsc r "SampleDur"        [] 1 0
sampleRate       r = mkOsc r "SampleRate"       [] 1 0

numAudioBuses :: Rate -> UGen
numBuffers :: Rate -> UGen
numControlBuses :: Rate -> UGen
numInputBuses :: Rate -> UGen
numOutputBuses :: Rate -> UGen
numRunningSynths :: Rate -> UGen
sampleDur :: Rate -> UGen
sampleRate :: Rate -> UGen
