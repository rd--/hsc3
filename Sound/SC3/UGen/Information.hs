module Sound.SC3.UGen.Information where

import Sound.SC3.UGen.UGen (UGen)
import Sound.SC3.UGen.Private (mkInfoUGen)

-- | Sample rate of synthesis server, frames per second.
sampleRate :: UGen
sampleRate = mkInfoUGen "SampleRate"

-- | Duration of one sample, seconds.
sampleDur :: UGen
sampleDur = mkInfoUGen "SampleDur"

-- | Duration of one sample, radians.
radiansPerSample :: UGen
radiansPerSample = mkInfoUGen "RadiansPerSample"

-- | Control rate of synthesis server, periods per second.
controlRate :: UGen
controlRate = mkInfoUGen "ControlRate"

-- | Sub-sample accurate scheduling offset.
subsampleOffset :: UGen
subsampleOffset = mkInfoUGen "SubsampleOffset"

-- | Number of allocated output audio rate buses.
numOutputBuses :: UGen
numOutputBuses = mkInfoUGen "NumOutputBuses"

-- | Number of allocated input audio rate buses.
numInputBuses :: UGen
numInputBuses = mkInfoUGen "NumInputBuses"

-- | Number of allocated audio rate buses.
numAudioBuses :: UGen
numAudioBuses = mkInfoUGen "NumAudioBuses"

-- | Number of allocated control rate buses.
numControlBuses :: UGen
numControlBuses = mkInfoUGen "NumControlBuses"

-- | Number of allocated buffers.
numBuffers :: UGen
numBuffers = mkInfoUGen "NumBuffers"

-- | Number of runnings synthesis nodes.
numRunningSynths :: UGen
numRunningSynths = mkInfoUGen "NumRunningSynths"
