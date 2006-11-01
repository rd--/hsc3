module Sound.SC3.UGen.Information where

import Sound.SC3.UGen.Rate (Rate(IR))
import Sound.SC3.UGen.UGen (UGen, mkOsc)

mkInfoUGen :: String -> UGen
mkInfoUGen name = mkOsc IR name [] 1 0

sampleRate :: UGen
sampleRate = mkInfoUGen "SampleRate"

sampleDur :: UGen
sampleDur = mkInfoUGen "SampleDur"

radiansPerSample :: UGen
radiansPerSample = mkInfoUGen "RadiansPerSample"

controlRate :: UGen
controlRate = mkInfoUGen "ControlRate"

subsampleOffset :: UGen
subsampleOffset = mkInfoUGen "SubsampleOffset"

numOutputBuses :: UGen
numOutputBuses = mkInfoUGen "NumOutputBuses"

numInputBuses :: UGen
numInputBuses = mkInfoUGen "NumInputBuses"

numAudioBuses :: UGen
numAudioBuses = mkInfoUGen "NumAudioBuses"

numControlBuses :: UGen
numControlBuses = mkInfoUGen "NumControlBuses"

numBuffers :: UGen
numBuffers = mkInfoUGen "NumBuffers"

numRunningSynths :: UGen
numRunningSynths = mkInfoUGen "NumRunningSynths"
