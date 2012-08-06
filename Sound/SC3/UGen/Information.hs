-- | Unit generators to access information related to the synthesis
--   environment.
module Sound.SC3.UGen.Information where

import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | Sample rate of synthesis server, frames per second.
sampleRate :: UGen
sampleRate = mkInfo "SampleRate"

-- | Duration of one sample, seconds.
sampleDur :: UGen
sampleDur = mkInfo "SampleDur"

-- | Duration of one sample, radians.
radiansPerSample :: UGen
radiansPerSample = mkInfo "RadiansPerSample"

-- | Control rate of synthesis server, periods per second.
controlRate :: UGen
controlRate = mkInfo "ControlRate"

-- | Sub-sample accurate scheduling offset.
subsampleOffset :: UGen
subsampleOffset = mkInfo "SubsampleOffset"

-- | Number of allocated output audio rate buses.
numOutputBuses :: UGen
numOutputBuses = mkInfo "NumOutputBuses"

-- | Number of allocated input audio rate buses.
numInputBuses :: UGen
numInputBuses = mkInfo "NumInputBuses"

-- | Number of allocated audio rate buses.
numAudioBuses :: UGen
numAudioBuses = mkInfo "NumAudioBuses"

-- | Number of allocated control rate buses.
numControlBuses :: UGen
numControlBuses = mkInfo "NumControlBuses"

-- | Number of allocated buffers.
numBuffers :: UGen
numBuffers = mkInfo "NumBuffers"

-- | Number of runnings synthesis nodes.
numRunningSynths :: UGen
numRunningSynths = mkInfo "NumRunningSynths"


-- | Poll value of input UGen when triggered.
poll :: UGen -> UGen -> UGen -> UGen -> UGen
poll t i l tr = mkFilter "Poll" ([t,i,tr] ++ unpackLabel l) 0

-- | Variant of 'poll' that generates an 'mrg' value with the input
-- signal at left.
poll' :: UGen -> UGen -> UGen -> UGen -> UGen
poll' t i l tr = mrg [i,poll t i l tr]
